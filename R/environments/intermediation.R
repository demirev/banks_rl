Intermediation <- R6Class(
  "An Economy with autonomous firms, savers, and banks",
  inherit = EconomyConstructor,
  public = list(
    
    createOptimizer = function() {
      "Initialize the optimizers"
      self$Optimizer$invest <- optim$Adam(self$DQN$invest$current$parameters())
      self$Optimizer$borrow <- optim$Adam(self$DQN$borrow$current$parameters())
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      self$Buffer$invest <- ReplayBuffer(bufferSize)
      self$Buffer$borrow <- ReplayBuffer(bufferSize)
      self$Buffer$depost <- ReplayBuffer(bufferSize)
      self$Buffer$withdraw <- ReplayBuffer(bufferSize)
      self$Buffer$loanrate <- ReplayBuffer(bufferSize)
      self$Buffer$depositrate <- ReplayBuffer(bufferSize)
      self$Buffer$approve <- ReplayBuffer(bufferSize)
      invisible(self)
    },
    
    stepBank = function(Approvals, LoanChanges, DepositChanges) {
      "Steps through banks' actions"
      
      # 0. Check for defaults
      
      # 1. Approve loans
      
      # 2. Adjust interest on deposits
      
      # 3. Adjust interest on loans
    },
    
    stepFirm = function(Investments, Borrowings) { 
      "Steps through firms' actions"
      
      # 0. Interest is paid
      self$repay()
      
      # 1. each firm applies for loans for their current investment
      self$processLoans(Borrowings)
      
      # 2. each firm keeps or discards their current opportunity
      self$invest(Investments)
      
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
      self$withdraw(Withdrawals)
      
      # 2. each customer deposits (or not) their savings
      self$deposit(Deposits)
      
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
          bank$depositRate
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
        borrow = rep(NA, numEpisodes)
      )
      
      resetFlag <- T
      
      # train
      for (episode in 1:numEpisodes) {
        
        # 0. set exploration rate
        epsilon <- generateEpsilon(episode)
        
        # 1. Banks act ----
        
        # 1.0 Update Info Set
        
        # 1.1 Update buffer
        if (!resetFlag) {
          pmap(
            list(
              self$OldInfoSets$Banks, 
              Approvals, 
              LoanChanges, 
              DepositChanges,
              self$Rewards$Banks, 
              self$InfoSets$Banks
            ), 
            function(state, action_a, action_l, action_d, reward, next_state) {
              self$Buffer$approve$push(state, which.max(action_a) - 1L, reward, next_state)
              self$Buffer$loanrate$push(state, which.max(action_l) - 1L, reward, next_state)
              self$Buffer$depositrate$push(state, which.max(action_d) - 1L, reward, next_state)
            }
          )
        }
        
        # 1.2 Make decisions
        Approvals <- lapply(
          self$InfoSets$Banks, 
          function(info) {
            decision <- self$DQN$approve$current$act(info, epsilon)
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
        self$stepBank(Approvals, LoanChanges, DepositChanges)
        
        # 2. Firms act ----
        
        # 2.0 Update Info Set
        
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
          Loss$approve[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$approve,
            self$DQN$approve$current, 
            self$DQN$approve$target,
            self$Optimizer$approve,
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
          update_target(self$DQN$approve$current, self$DQN$approve$target)
        }
        
        if (verbose == 1) {
          cat(".")
        } else if (verbose == 2) {
          print(self$EpisodeHistory[[length(self$EpisodeHistory)]])
        }
      }
      
      return(Loss)
      
    },
    
    getInfoSet = function() {
      "Generates a state vector for each household"
      
      # Get some info for each bank
      bankStates <- lapply(
        self$Banks,
        function(bank) {
          bank$getState()
        }
      ) %>%
        reduce(c)
      
      # Combine it with the firm's holdings
      decisionStates <- lapply(
        self$Firms,
        function(firm) {
          c(bankStates, firm$getState())
        }
      )
      
      return(decisionStates)
    },
    
    
    repay = function() {
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
    
    processLoans = function(Borrowings) {  
      "Banks decide whether to accept or reject a loan" 
      
      # gather all project info in one list
      Applications <- lapply(
        self$Firms,
        function(firm) {
          firm$application
        }
      )
      
      # create a vector of accept / reject decisions and adjust bank balances
      Decisions <- map2(
        Borrowings, Applications,
        function(borrowing, application) {
          if (borrowing[length(borrowing)] == 1) {
            # last index is no loan
            return(0)
          } else {
            bankIndex <- which(borrowing == 1)
            return(self$Banks[[bankIndex]]$process(application))
          }
        }
      )
      
      # initiate approved projects
      pmap(
        list(self$Firms, Decisions, Borrowings),
        function(firm, decision, bankId) {
          firm$rollProject(decision, which(bankId == 1))
        }
      )
      
      return(self)
      
    },
    
    invest = function(Investments) {
      "Firms decide whether to invest in a project or discard it" 
      amounts <- map2(
        self$Firms, Investments,
        function(firm, decision) {
          # 1 is invest, 0 discard
          firm$invest(decision)
        }
      ) 
    },
    
    defaults = function(bankOutcomes) {
      "Decreases households outstanding deposits given a list of defaulted banks"
      lapply(
        self$Firms,
        function(firm) {
          firm$bankDefault(bankOutcomes)
        }
      )
    }
    
  )
)
