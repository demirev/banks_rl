Intermediation <- R6Class(
  "An Economy with autonomous firms, savers, and banks",
  inherit = EconomyConstructor,
  public = list(
    
    InfoSets = list(Bank = list(), Households = list(), Firms = list()),
    OldInfoSets = list(Bank = list(), Households = list(), Firms = list()),
    Rewards = list(Bank = list(), Households = list(), Firms = list()),
    
    createOptimizer = function() {
      "Initialize the optimizers"
      self$Optimizer$invest <- optim$Adam(self$DQN$invest$current$parameters())
      self$Optimizer$borrow <- optim$Adam(self$DQN$borrow$current$parameters())
      self$Optimizer$deposit <- optim$Adam(self$DQN$deposit$current$parameters())
      self$Optimizer$withdraw <- optim$Adam(self$DQN$withdraw$current$parameters())
      self$Optimizer$loanrate <- optim$Adam(self$DQN$loanrate$current$parameters())
      self$Optimizer$depositrate <- optim$Adam(self$DQN$depositrate$current$parameters())
      self$Optimizer$approverate <- optim$Adam(self$DQN$approverate$current$parameters())
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      self$Buffer$invest <- ReplayBuffer(bufferSize)
      self$Buffer$borrow <- ReplayBuffer(bufferSize)
      self$Buffer$deposit <- ReplayBuffer(bufferSize)
      self$Buffer$withdraw <- ReplayBuffer(bufferSize)
      self$Buffer$loanrate <- ReplayBuffer(bufferSize)
      self$Buffer$depositrate <- ReplayBuffer(bufferSize)
      self$Buffer$approverate <- ReplayBuffer(bufferSize)
      invisible(self)
    },
    
    stepBank = function(ApproveChanges, LoanChanges, DepositChanges) {
      "Steps through banks' actions"
      
      # 0. Check for defaults
      bankOutcomes <- lapply(
        self$Banks,
        function(bank) {
          bank$isDefault() # dummy actions by each bank
        }
      )
      self$bankDefaults(bankOutcomes)
      
      # 1. Approve loans
      self$processLoans()
      
      # 2. Adjust interest on deposits and loans
      pmap(
        list(self$Banks, LoanChanges, DepositChanges, ApproveChanges),
        function(bank, loanChange, depositChange, approveChange) {
          bank$adjustLoans(loanChange)
          bank$adjustDeposits(depositChange)
          bank$adjustApprovals(approveChange)
        }
      )
      
      invisible(self)
    },
    
    stepFirm = function(Investments, Borrowings) { 
      "Steps through firms' actions"
      
      # 0. Interest is paid
      self$firmsRepay()
      
      # 1. each firm applies for loans for their current investment
      self$firmsApply(Borrowings)
      
      # 2. each firm keeps or discards their current opportunity
      self$firmsInvest(Investments)
      
      # 3. firms consume whatever is not invested
      Consumption <- lapply(
        self$Firms,
        function(firm) {
          firm$consume()
        }
      )
      
      invisible(self)
    },
    
    stepHousehold = function() {
      "Steps through households' actions"
      
      # last element corresponds to no action (no withdrawal or deposit) 
      Withdrawals <- lapply(Withdrawals, function(x) x[-length(x)])
      Deposits    <- lapply(Deposits, function(x) x[-length(x)])
      
      # 1. each bank loses withdrawn money
      self$householdsWithdraw(Withdrawals)
      
      # 2. each customer deposits (or not) their savings
      self$householdsDeposit(Deposits)
      
      # 3. Households consume whatever is not deposited
      Consumption <- lapply(
        self$Households,
        function(household) {
          household$consume()
        }
      )
      
      # 4. Interest is received
      interestRates <- sapply(
        self$Banks, 
        function(bank) {
          bank$payDeposits()
        }
      )
      lapply(
        self$Households,
        function(household) {
          household$getInterest(interestRates)
        }
      )
      
      invisible(self)
    },
    
    updateHistory = function(type = "episode") {
      # all deposits for history
      allProjects <- lapply(
        self$Firms,
        function(firm) {
          firm$ProjectLoans
        }
      ) %>%
        reduce(rbind)
      
      # bank details
      bankBalances <- lapply(
        self$Banks,
        function(bank) {
          tibble(
            type = bank$rating,
            depositRate = bank$depositRate,
            loanRate = bank$loanRate,
            defaultCounter = bank$defaultCounter,
            deposits = bank$deposits,
            loans = bank$loans,
            reserves = bank$reserves,
            capital = bank$capital
          )
        }
      ) %>%
        reduce(rbind)
      
      if (type == "episode") {
        # update episode history
        self$EpisodeHistory[[length(self$EpisodeHistory) + 1]] <- list(
          projects = allProjects,
          banks    = bankBalances
        )
      } else if (type == "full") {
        # add current state to full history
        self$FullHistory[[length(self$FullHistory) + 1]] <- self$EpisodeHistory
      }
      
      return(T)
    },
    
    train = function(
      numEpisodes = 10000, 
      resetProb = 0.001, 
      batch_size = 256,
      updateFreq = 200,
      verbose = 0
    ) {
      "Train the networks"
      # reset the economy
      
      Loss <- list(
        invest = rep(NA, numEpisodes), 
        borrow = rep(NA, numEpisodes),
        deposit = rep(NA, numEpisodes),
        withdraw = rep(NA, numEpisodes),
        loanrate = rep(NA, numEpisodes),
        depositrate = rep(NA, numEpisodes),
        approve = rep(NA, numEpisodes)
      )
      
      resetFlag <- T
      
      # train
      for (episode in 1:numEpisodes) {
        epsilon <- generateEpsilon(episode)
        
        # 0. Productions takes place
        self$produce()
        
        # 1. Banks act ----
        
        # 1.0 Update Info Set
        self$getInfoSet("Banks")
        
        # 1.1 Update buffer
        if (!resetFlag) {
          pmap(
            list(
              self$OldInfoSets$Banks, 
              ApproveChanges, 
              LoanChanges, 
              DepositChanges,
              self$Rewards$Banks, 
              self$InfoSets$Banks
            ), 
            function(state, action_a, action_l, action_d, reward, next_state) {
              self$Buffer$approverate$push(state, which.max(action_a) - 1L, reward, next_state)
              self$Buffer$loanrate$push(state, which.max(action_l) - 1L, reward, next_state)
              self$Buffer$depositrate$push(state, which.max(action_d) - 1L, reward, next_state)
            }
          )
        }
        
        # 1.2 Make decisions
        ApproveChanges <- lapply(
          self$InfoSets$Banks, 
          function(info) {
            decision <- self$DQN$approverate$current$act(info, epsilon)
            return(decision)
          }
        )
        LoanChanges <- lapply(
          self$InfoSets$Banks,
          function(info) {
            decision <- self$DQN$loanrate$current$act(info, epsilon)
            return(decisions)
          }
        )
        DepositChanges <- lapply(
          self$InfoSets$Banks,
          function(info) {
            decision <- self$DQN$depositrate$current$act(info, epsilon)
          }
        )
        
        # 1.3 Interact with economy
        self$stepBank(ApproveChanges, LoanChanges, DepositChanges)
        
        # 2. Firms act ----
        
        # 2.0 Update Info Set
        self$getInfoSet("Firms")
        
        # 2.1 Update buffer
        if (!resetFlag) {
          pmap(
            list(
              self$OldInfoSets$Firms, 
              Investments, 
              Borrowings, 
              self$Rewards$Firms, 
              self$InfoSets$Firms
            ), 
            function(state, action_i, action_b, reward, next_state) {
              self$Buffer$invest$push(state, which.max(action_i) - 1L, reward, next_state)
              self$Buffer$borrow$push(state, which.max(action_b) - 1L, reward, next_state)
            }
          )
        }
        
        # 2.2 Make decisions
        Investments <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$invest$current$act(info, epsilon)
            vec <- c(0, 0)
            vec[decision + 1] <- 1
            return(decision)
          }
        )
        Borrowings <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$borrow$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 2.3 Interact with economy
        self$stepFirm(Investments, Borrowings)
        
        # 3. Households act ----
        
        # 3.0 Update Info Set
        self$getInfoSet("Households")
        
        # 3.1 Update buffer
        if (!resetFlag) {
          pmap(
            list(
              self$OldInfoSets$Firms, 
              Withdrawals, 
              Deposits, 
              self$Rewards$Firms, 
              self$InfoSets$Firms
            ), 
            function(state, action_w, action_d, reward, next_state) {
              self$Buffer$withdraw$push(state, which.max(action_w) - 1L, reward, next_state)
              self$Buffer$deposit$push(state, which.max(action_d) - 1L, reward, next_state)
            }
          )
        }
        
        # 3.2 Make decisions
        Withdrawals <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$withdraw$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1) # +1 to allow for inaction 
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        Deposits <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$deposit$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 3.3 Interact with economy
        self$stepHoushold(Withdrawals, Deposits)
        
        
        if (runif(1) <= resetProb) {
          self$reset()
          resetFlag = T
          self$updateHistory("full")
          self$EpisodeHistory <- list()
        } else {
          resetFlag = F
        }
        
        # 4. GD Step
        if (self$Buffer$invest$getLen() > batch_size) {
          Loss$invest[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$invest,
            self$DQN$invest$current, 
            self$DQN$invest$target,
            self$Optimizer$invest,
            self$beta
          )$detach()$numpy()
          Loss$borrow[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$borrow,
            self$DQN$borrow$current, 
            self$DQN$borrow$target,
            self$Optimizer$borrow,
            self$beta
          )$detach()$numpy()
          Loss$withdraw[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$withdraw,
            self$DQN$withdraw$current, 
            self$DQN$withdraw$target,
            self$Optimizer$withdraw,
            self$beta
          )$detach()$numpy()
          Loss$deposit[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$deposit,
            self$DQN$deposit$current, 
            self$DQN$deposit$target,
            self$Optimizer$deposit,
            self$beta
          )$detach()$numpy()
          Loss$loanrate[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$loanrate,
            self$DQN$loanrate$current, 
            self$DQN$loanrate$target,
            self$Optimizer$loanrate,
            self$beta
          )$detach()$numpy()
          Loss$depositrate[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$depositrate,
            self$DQN$depositrate$current, 
            self$DQN$depositrate$target,
            self$Optimizer$depositrate,
            self$beta
          )$detach()$numpy()
          Loss$approverate[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$approverate,
            self$DQN$approverate$current, 
            self$DQN$approverate$target,
            self$Optimizer$approverate,
            self$beta
          )$detach()$numpy()
        }
        
        # 4. Update Target Network
        if (episode %% updateFreq == 0) {
          update_target(self$DQN$invest$current, self$DQN$invest$target)
          update_target(self$DQN$borrow$current, self$DQN$borrow$target)
          update_target(self$DQN$deposit$current, self$DQN$deposit$target)
          update_target(self$DQN$withdraw$current, self$DQN$withdraw$target)
          update_target(self$DQN$loanrate$current, self$DQN$loanrate$target)
          update_target(self$DQN$depositrate$current, self$DQN$depositrate$target)
          update_target(self$DQN$approverate$current, self$DQN$approverate$target)
        }
        
        if (verbose == 1) {
          cat(".")
        } else if (verbose == 2) {
          print(self$EpisodeHistory[[length(self$EpisodeHistory)]])
        }
      }
      
      return(Loss)
      
    },
    
    getInfoSet = function(type = "Banks") {
      "Generates a state vector for each agent"
      
      if (type == "Banks") {
        self$OldInfoSets$Banks <- self$InfoSets$Banks
        
        # Aggregate economy state
        # TO DO
        
        self$InfoSets$Banks <- map(
          self$Banks,
          function(bank) {
            bank$getState()
          }
        )
        
      } else if (type == "Firms") {
        self$OldInfoSets$Firms <- self$InfoSets$Firms
        
        # Get aggregate economy state
        # TO DO
        
        # Get some info for each bank
        bankStates <- map(
          self$Banks,
          function(bank) {
            bank$getState()
          }
        ) %>%
          reduce(c)
        
        # Combine it with the firm's holdings
        self$InfoSets$Firms <- map(
          self$Firms,
          function(firm) {
            c(bankStates, firm$getState())
          }
        )
        
      } else if (type == "Households") {
        self$OldInfoSets$Households <- self$InfoSets$Households
        
        # Get aggregate economy state
        # TO DO
        
        # Get some info for each bank
        bankStates <- map(
          self$Banks,
          function(bank) {
            bank$getState()
          }
        ) %>%
          reduce(c)
        
        # Combine it with the household's state
        self$InfoSets$Households <- map(
          self$Households,
          function(household) {
            c(bankStates, household$getState())
          }
        )
        
      } else {
        stop("Wrong type")
      }
      
      invisible(self)
    },
    
    produce = function() {
      "Production takes place"
      
      # aggregate inputs
      capital <- self$Firms %>%
        map("capital") %>%
        reduce(sum)
      
      labor <- self$Households %>%
        map("labor") %>%
        reduce(sum)
      
      # carry out production
      self$ProductionFunction$shock()
      
      output <- self$ProductionFunction$produce(capital, labor)
      wage <- self$ProductionFunction$wage(capital, labor)
      rate <- self$ProductionFunction$rate(capital, labor)
      
      # give out factor products (and depreciate)
      self$Households %>%
        map(function(houshold) {
          household$cash <- household$cash + household$labor * wage
        })
      
      self$Firms %>%
        map(function(firm) {
          firm$cash <- firm$cash + firm$capital * rate
          firm$capital <- firm$capital * (1 - self$depreciation)
        })
      
    },
    
    bankDefaults = function(Outcomes) {
      "Resolves pottential bank defaults"
      
      # reset bank balances
      self$Banks %>%
        filter(Outcomes) %>% # 1 is default
        map(function(bank) {
          bank$default()
        })
      
      # reduce housholds' deposits
      self$Households %>%
        map(
          function(household) {
            household$holdings[Outcomes == 1] <- 0 # loses savings
          }
        )
      
      # reduce firms' borrowing
      self$Firms %>%
        map(
          function(firm) {
            firm$bankDefaults(Outcomes == 1)
          }
        )
      
      invisible(self) 
    },
    
    processLoans = function() {
      "Approves or rejects loans"
      
      # Crate a vector of accept / reject decisions for each application
      Decisions <- self$Firms %>% 
        map(
          function(firm) {
            application <- firm$application
            if (application$bank != -1) {
              decision <- self$Banks[[application$bank]]$process(application)
              firm$rollProject(decision)
            } else {
              firm$rollProject(1) # no loan required
            }
          }
        )
      
      # clean banks' queued loan applications
      self$Banks %>%
        map(function(bank) {
          bank$applications <- tibble()
        })
      
      invisible(self)
    },   
    
    firmsRepay = function() {
      "Loan Payments from Firms to Banks"
      repayments <- lapply(
        self$Firms,
        function(firm) {
          firm$payInterest(nBanks = length(self$Banks))
        }
      )
      
      principal <- lapply(
        repayments, 
        function(repayment) repayment$principal
      ) %>%
        reduce(cbind) %>%
        rowSums
      interest <- lapply(
        repayments, 
        function(repayment) repayment$interest
      ) %>%
        reduce(cbind) %>%
        rowSums 
      
      pmap(
        list(self$Banks, principal, interest),
        function(bank, principal, interest) {
          bank$loans    <- bank$loans - principal
          bank$reserves <- bank$reserves + principal + interest
        }
      )
      
      invisible(self)
    },
    
    firmsApply = function(Borrowings) {
      "Firms choose to which bank (if any) to apply for a loan"
      
      # firms assign a bank id to their projects
      map2(
        self$Firms, Borrowings,
        function(firm, decision) {
          firm$borrow(decision)
        }
      )
      
      # aggregate projects applications to add to bank queues
      Applications <- self$Firms %>% 
        map("application") %>%
        reduce(rbind) %>%
        split(.$bank) %>%
        arrange(bank) %>%
        right_join(tibble(bank = 1:length(self$Banks)))
      
      # add to bank queue
      self$Banks %>%
        map2(
          Applications, 
          function(bank, applications) {
            bank$applications <- applications
          }
        )
      
      invisible(self)
    },
    
    firmsInvest = function(Investments) {
      "Firms decide whether to invest in a project or discard it" 
      map2(
        self$Firms, Investments,
        function(firm, decision) {
          # 1 is invest, 0 discard
          firm$invest(decision)
        }
      )
      
      invisible(self)
    },
    
    householdsWithdraw = function(Withdrawals) {
      "Households withdraw some of their deposits"
      Amounts <- map2(
        self$Households, Withdrawals,
        function(household, decision) {
          # vector with amount withdrawn from each bank
          amount <- household$holdings * (decision == 1)
          
          # set withdrawals to zero
          household$holdings[decision == 1] <- 0
          
          return(amount)
        }
      ) %>%
        reduce(cbind) %>%
        rowSums
      
      map2(
        self$Banks, Amounts,
        function(bank, amount){
          bank$deposits <- bank$deposits - amount
          bank$reserves <- bank$reserves - amount
        }
      )
    },
    
    householdsDeposit = function(Deposits) {
      "Households deposit their cash"
      
      Amounts <- map2(
        self$Households, Deposits,
        function(household, decision) {
          # vector with amount deposited in each bank (all but one are zero)
          res <- household$cash * (decision == 1)
          household$holdings <- household$holdings + res
          if (sum(decision) != 0) household$cash <- 0
          return(res)
        }
      ) %>%
        reduce(cbind) %>%
        rowSums
      
      map2(
        self$Banks, Amounts,
        function(bank, amount){
          bank$deposits <- bank$deposits + amount
          bank$reserves <- bank$reserves + amount
        }
      )
    }
    
    
  )
)
