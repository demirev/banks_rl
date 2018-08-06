Intermediation <- R6Class(
  "An Economy with autonomous firms, savers, and banks",
  inherit = EconomyConstructor,
  public = list(
    
    output = NULL,
    wage = NULL,
    rate = NULL,
    ProductionFunction = NULL,
    depreciation = NULL,
    dqnGen = NULL, 
    dqnInd = NULL, # needed for saving/loading weights
    dqnData = NULL, # holds saved weights
    InfoSets = list(Banks = list(), Households = list(), Firms = list()),
    OldInfoSets = list(Banks = list(), Households = list(), Firms = list()),
    Rewards = list(Banks = list(), Households = list(), Firms = list()),
    
    initialize = function(
      households = list(), 
      banks = list(), 
      firms = list(),
      dqnGen, 
      productionFunction = CobbDouglass$new(),
      lossFunction, 
      bufferSize = 1000,
      depreciation = 1
    ) {
      
      self$dqnGen = dqnGen
      self$dqnInd = dqnGen()$Indices
      
      super$initialize(
        households,
        banks, 
        firms,
        dqnGen()$Network, 
        lossFunction, 
        bufferSize
      )
      
      self$ProductionFunction = productionFunction
      self$depreciation = depreciation
      
    },
    
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
      
      # 3. Bank management gets excess capital as a reward
      self$Rewards$Banks <- map(
        self$Banks,
        function(bank) {
          bank$consume()
        }
      )
      
      invisible(self)
    },
    
    stepFirm = function(Investments, Borrowings) { 
      "Steps through firms' actions"
      
      # 0. Projects give capital or default
      self$firmsResolveProjects()
      
      # 1. Interest is paid
      self$firmsRepay()
      
      # 2. each firm keeps or discards their current opportunity
      self$firmsInvest(Investments)
      
      # 3. each firm applies for loans for their current investment
      self$firmsApply(Borrowings)
      
      # 4. firms consume whatever is not invested
      self$Rewards$Firms <- map(
        self$Firms,
        function(firm) {
          firm$consume()
        }
      )
      
      invisible(self)
    },
    
    stepHousehold = function(Withdrawals, Deposits) {  
      "Steps through households' actions"
      
      # last element corresponds to no action (no withdrawal or deposit) 
      Withdrawals <- lapply(Withdrawals, function(x) x[-length(x)])
      Deposits    <- lapply(Deposits, function(x) x[-length(x)])
      
      # 1. each bank loses withdrawn money
      self$householdsWithdraw(Withdrawals)
      
      # 2. each customer deposits (or not) their savings
      self$householdsDeposit(Deposits)
      
      # 3. Households consume whatever is not deposited
      self$Rewards$Households <- map(
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
          household$receiveInterest(interestRates)
        }
      )
      
      invisible(self)
    },
    
    updateHistory = function(type = "episode") {
      
      # macro variables
      Macro <- list(
        ouput = self$output,
        wage = self$wage,
        rate = self$rate
      )
      
      # all deposits for history
      Projects <- lapply(
        self$Firms,
        function(firm) {
          firm$Projects
        }
      ) %>%
        reduce(rbind)
      
      # bank details
      Banks <- lapply(
        self$Banks,
        function(bank) {
          tibble(
            depositRate = bank$depositRate,
            loanRate = bank$loanRate,
            approvalRate = bank$approvalRate,
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
          macro    = Macro,
          projects = Projects,
          banks    = Banks
        )
      } else if (type == "full") {
        # add current state to full history
        self$FullHistory[[length(self$FullHistory) + 1]] <- self$EpisodeHistory
      }
      
      return(T)
    },
    
    reset = function() {
      "Resets the economy"
      super$reset()
      self$InfoSets = list(Bank = list(), Households = list(), Firms = list())
      self$OldInfoSets = list(Bank = list(), Households = list(), Firms = list())
      self$Rewards = list(Bank = list(), Households = list(), Firms = list())
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
        approverate = rep(NA, numEpisodes)
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
            vec <- rep(0, 5)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        LoanChanges <- lapply(
          self$InfoSets$Banks,
          function(info) {
            decision <- self$DQN$loanrate$current$act(info, epsilon)
            vec <- rep(0, 5)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        DepositChanges <- lapply(
          self$InfoSets$Banks,
          function(info) {
            decision <- self$DQN$depositrate$current$act(info, epsilon)
            vec <- rep(0, 5)
            vec[decision + 1] <- 1
            return(vec)
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
          self$InfoSets$Firms, 
          function(info) {
            decision <- self$DQN$invest$current$act(info, epsilon)
            vec <- c(0, 0)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        Borrowings <- lapply(
          self$InfoSets$Firms, 
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
              self$OldInfoSets$Households, 
              Withdrawals, 
              Deposits, 
              self$Rewards$Households, 
              self$InfoSets$Households
            ), 
            function(state, action_w, action_d, reward, next_state) {
              self$Buffer$withdraw$push(state, which.max(action_w) - 1L, reward, next_state)
              self$Buffer$deposit$push(state, which.max(action_d) - 1L, reward, next_state)
            }
          )
        }
        
        # 3.2 Make decisions
        Withdrawals <- lapply(
          self$InfoSets$Households, 
          function(info) {
            decision <- self$DQN$withdraw$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1) # +1 to allow for inaction 
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        Deposits <- lapply(
          self$InfoSets$Households, 
          function(info) {
            decision <- self$DQN$deposit$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 3.3 Interact with economy
        self$stepHousehold(Withdrawals, Deposits)
        
        # update history and reset if needed
        self$updateHistory("episode")
        
        if (runif(1) <= resetProb) {
          self$reset()
          resetFlag = TRUE
          self$updateHistory("full")
          self$EpisodeHistory <- list()
        } else {
          resetFlag = FALSE
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
        }
        if (self$Buffer$borrow$getLen() > batch_size) {
          Loss$borrow[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$borrow,
            self$DQN$borrow$current, 
            self$DQN$borrow$target,
            self$Optimizer$borrow,
            self$beta
          )$detach()$numpy()
        }
        if (self$Buffer$withdraw$getLen() > batch_size) {
          Loss$withdraw[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$withdraw,
            self$DQN$withdraw$current, 
            self$DQN$withdraw$target,
            self$Optimizer$withdraw,
            self$beta
          )$detach()$numpy()
        }
        if (self$Buffer$deposit$getLen() > batch_size) {
          Loss$deposit[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$deposit,
            self$DQN$deposit$current, 
            self$DQN$deposit$target,
            self$Optimizer$deposit,
            self$beta
          )$detach()$numpy()
        }
        if (self$Buffer$loanrate$getLen() > batch_size) {
          Loss$loanrate[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$loanrate,
            self$DQN$loanrate$current, 
            self$DQN$loanrate$target,
            self$Optimizer$loanrate,
            self$beta
          )$detach()$numpy()
        }
        if (self$Buffer$depositrate$getLen() > batch_size) {
          Loss$depositrate[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$depositrate,
            self$DQN$depositrate$current, 
            self$DQN$depositrate$target,
            self$Optimizer$depositrate,
            self$beta
          )$detach()$numpy()
        }
        if (self$Buffer$approverate$getLen() > batch_size) {
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
      
      economyStates <- self$getEconomyState()
      
      if (type == "Banks") {
        self$OldInfoSets$Banks <- self$InfoSets$Banks
        
        # Get some info for other banks
        bankStates <- map(
          self$Banks,
          function(bank) {
            bank$getState()
          }
        ) %>%
          reduce(c)
        
        # TO DO - add summaries?
        
        # combine with each bank's individual state
        self$InfoSets$Banks <- map(
          self$Banks,
          function(bank) {
            c(bank$getState(), bankStates, economyStates)
          }
        )
        
      } else if (type == "Firms") {
        self$OldInfoSets$Firms <- self$InfoSets$Firms
        
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
            c(firm$getState(), bankStates, economyStates)
          }
        )
        
      } else if (type == "Households") {
        self$OldInfoSets$Households <- self$InfoSets$Households
        
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
            c(household$getState(), bankStates, economyStates)
          }
        )
          
      } else {
        stop("Wrong type")
      }
      
      invisible(self)
    },
    
    getEconomyState = function() {
      "Some state variables that summarize the economy"
      c(
        "ouput" = self$output,
        "wage" = self$wage,
        "rate" = self$rate
      )
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
      
      self$output <- self$ProductionFunction$produce(capital, labor)
      self$wage <- self$ProductionFunction$wage(capital, labor)
      self$rate <- self$ProductionFunction$rate(capital, labor)
      
      # give out factor products (and depreciate)
      self$Households %>%
        map(function(household) {
          household$receiveWage(self$wage)
        })
      
      self$Firms %>%
        map(function(firm) {
          firm$receiveRate(self$rate, self$depreciation)
        })
      
      invisible(self)
    },
    
    bankDefaults = function(Outcomes) { 
      "Resolves pottential bank defaults"
      
      # reset bank balances
      self$Banks %>% 
        map2(Outcomes, function(bank,outcome) {
          if (as.logical(outcome)) bank$reset()
        }) # 1 is default
      
      # reduce housholds' deposits
      self$Households %>%
        map(
          function(household) {
            household$bankDefaults(Outcomes == 1)
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
            if (nrow(application) != 0) {
              decision <- self$Banks[[application$bank]]$processLoan(application)
              firm$rollProject(decision)
            } else {
              return(-1)
            }
          }
        )
      
      # clean banks' queued loan applications
      self$Banks %>%
        map(function(bank) {
          bank$clearQueue()
        })
      
      invisible(self)
    },   
    
    firmsResolveProjects = function() {
      "Each Firm's Projects are advanced 1 step"
      self$Firms %>%
        map(function(firm){
          firm$resolveProject()
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
          bank$receivePayment(principal, interest)
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
        arrange(bank) %>%
        right_join(tibble(bank = 1:length(self$Banks)), by = "bank") %>%
        split(.$bank) %>%
        map(function(bnk){
          if (nrow(bnk) == 1 & is.na(bnk$amount[1])) {
            # if no loans to this particular bank bnk will be a 1-row tibble with 
            # all fields NA except bank
            bnk <- bnk[0,]
          }
        })
      
      # add to bank queue
      self$Banks %>%
        map2(
          Applications, 
          function(bank, applications) {
            bank$addQueue(applications)
          }
        )
      
      invisible(self)
    },
    
    firmsInvest = function(Investments) {
      "Firms decide whether to invest in a project or discard it" 
      map2(
        self$Firms, Investments,
        function(firm, decision) {
          # position 1 is discard, position 2 is invest
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
          household$requestWithdrawal(decision)
        }
      ) %>%
        reduce(cbind) %>%
        rowSums
      
      AmountsPaid <- map2(
        self$Banks, Amounts,
        function(bank, amount){
          bank$payWithdrawal(amount)
        }
      ) %>% # precentage of requested rapyment
        unlist
      
      map2(
        self$Households, Withdrawals,
        function(household, decision) {
          household$receiveWithdrawal(decision, AmountsPaid)
        }
      )
      
    },
    
    householdsDeposit = function(Deposits) { 
      "Households deposit their cash"
      
      Amounts <- map2(
        self$Households, Deposits,
        function(household, decision) {
          # vector with amount deposited in each bank (all but one are zero)
          household$deposit(decision)
        }
      ) %>%
        reduce(cbind) %>%
        rowSums
      
      map2(
        self$Banks, Amounts,
        function(bank, amount){
          bank$receiveDeposit(amount)
        }
      )
    },
    
    saveWeights = function() {
      "Saves the weights of the DQNs as R matrices"
      self$dqnData <- self$DQN %>%
        map2(self$dqnInd, function(networkType, indexType){
          map2(networkType, indexType, function(network, index){
            index %>%
              map(function(ind){
                list(
                  bias = network$layers[ind]$bias$data$numpy(),
                  weight = network$layers[ind]$weight$data$numpy()
                )
              })
          })
        }) # this looks ugly but should do the trick for now
    },
    
    loadWeights = function() {
      "Assigns weights to the DQNs from R matrices"
      if (is.null(self$dqnData)) {
        stop("No weights saved")
      }
      
      self$DQN <- pmap(
        list(self$DQN, self$dqnInd, self$dqnData),
        function(networkType, indexType, dataType) {
          pmap(
            list(networkType, indexType, dataType),
            function(network, index, data) {
              tryCatch({
                index %>%
                  map2(data, function(ind, dta){
                    network$layers[ind]$bias$data = torch$tensor(dta$bias, dtype = torch$float)
                  })
              }, error = function(e) {
                NULL
              }) # tryCatch because of phantom errors
              tryCatch({
                index %>%
                  map2(data, function(ind, dta){
                    network$layers[ind]$weight$data = torch$tensor(dta$weight, dtype = torch$float)
                  })
              }, error = function(e) {
                NULL
              })
              network
            }
          )
        }
      )
    },
    
    reloadNetworks = function() {
      "Resets the networks if the object is loaded as an .Rdata file"
      self$DQN <- self$dqnGen()$Network
      self$loadWeights()
    }
    ####
  )
)
