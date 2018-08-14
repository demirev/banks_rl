Intermediation <- R6Class(
  "An Economy with autonomous firms, savers, and banks",
  inherit = EconomyConstructor,
  public = list(
    
    file = NULL,
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
    Queues = list(),
    LossData = NULL, # total loss
    
    initialize = function(
      file,
      households = list(), 
      banks = list(), 
      firms = list(),
      dqnGen, 
      productionFunction = CobbDouglass$new(),
      lossFunction, 
      bufferSize = 1000,
      depreciation = 1
    ) {
      
      self$file = file
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
      
      self$saveWeights()
      saveRDS(self, file = self$file)
    },
    
    createOptimizer = function() {
      "Initialize the optimizers"
      self$Optimizer$bank <- optim$Adam(self$DQN$bank$current$parameters(), lr = 0.001, weight_decay = 0.2)
      self$Optimizer$firm <- optim$Adam(self$DQN$firm$current$parameters(), lr = 0.001, weight_decay = 0.2)
      self$Optimizer$household <- optim$Adam(self$DQN$household$current$parameters(), lr = 0.001, weight_decay = 0.2)
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      self$Buffer$bank <- ReplayBuffer(bufferSize)
      self$Buffer$firm <- ReplayBuffer(bufferSize)
      self$Buffer$household <- ReplayBuffer(bufferSize)
      invisible(self)
    },
    
    stepBank = function(bankActions) { 
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
      
      # nn output to the format expeected from bank$adjust...
      Changes <- getChanges(bankActions)
      
      pmap(
        list(self$Banks, Changes),
        function(bank, change) {
          bank$adjustLoans(change$loanChange)
          bank$adjustDeposits(change$depositChange)
          bank$adjustApprovals(change$approveChange)
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
    
    stepFirm = function(firmActions) {
      "Steps through firms' actions"
      
      # 0. Projects give capital or default
      self$firmsResolveProjects()
      
      # 1. Interest is paid
      self$firmsRepay()
      
      # 2. each firm keeps or discards their current opportunity
      Investments <- map(firmActions, function(x) x[1:2]) # first two slots
      # represent invest/forgo and cash/loan
      self$firmsInvest(Investments)
      
      # 3. each firm applies for loans for their current investment
      Borrowings <- map(firmActions, function(x) x[3:length(x)]) # last actions
      # represent choice of bank for lending
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
    
    stepHousehold = function(householdActions) {
      "Steps through households' actions"
      
      # convert householdActions to the format expected by the
      # household methods Withdraw and deposit
      Withdrawals <- map2(
        householdActions, self$Households,
        function(action, household) {
          if (action[1] == 1) { # represents no change in deposits
            return(rep(0, length(self$Banks)))
          } else if (action[2] == 1) { # no withdrawal, just deposit in same bnak
            return(rep(0, length(self$Banks)))
          } else { # anything else means withdraw from current bank
            vec <- rep(0, length(self$Banks))
            vec[which(household$holdings > 0)] <- 1
            return(vec)
          }
        }
      )
      
      Deposits <- map2(
        householdActions, self$Households,
        function(action, household) {
          if (action[2] == 1) { # deposit in same bank as currently
            vec <- rep(0, length(self$Banks))
            vec[which(household$holdings > 0)] <- 1
            return(vec)
          } else {
            return(action[4:length(action)]) # these slots
            # represent deposits in new bank (all 0s allowed)
          } 
        }
      )
      
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
    
    updateHistory = function(Decisions, type = "episode") {
      
      # macro variables
      Macro <- list(
        output = self$output,
        wage = self$wage,
        rate = self$rate,
        productivity = self$ProductionFunction$productivity,
        capital = self$Firms %>% map("capital") %>% reduce(sum)
      )
      
      # all projects for history
      Projects <- map(
        self$Firms,
        function(firm) {
          firm$Projects
        }
      ) %>%
        reduce(rbind)
      
      # bank details
      Banks <- map(
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
      
      # household's wealth
      Deposits <- self$Households %>%
        map("holdings")
      
      if (type == "episode") {
        # update episode history
        self$EpisodeHistory[[length(self$EpisodeHistory) + 1]] <- list(
          macro     = Macro,
          projects  = Projects,
          deposits  = Deposits,
          banks     = Banks,
          rewards   = self$Rewards,
          decisions = Decisions,
          queues    = self$Queues
        )
      } else if (type == "full") {
        # add current state to full history
        self$FullHistory[[length(self$FullHistory) + 1]] <- self$EpisodeHistory
      }
      
      return(T)
    },
    
    reset = function() {
      "Resets the economy"
      cat("-")
      super$reset()
      self$InfoSets = list(Banks = list(), Households = list(), Firms = list())
      self$OldInfoSets = list(Banks = list(), Households = list(), Firms = list())
      self$Rewards = list(Banks = list(), Households = list(), Firms = list())
    },
    
    train = function(
      numEpisodes = 10000,
      resetProb = 0.001, 
      batch_size = 256,
      updateFreq = 200,
      verbose = 0,
      saveEvery = 0,
      fixed = FALSE
    ) {
      "Train the networks" 
      # reset the economy
      
      Loss <- list(
        bank = rep(NA, numEpisodes), 
        firm = rep(NA, numEpisodes),
        household = rep(NA, numEpisodes)
      )
      
      resetFlag <- T
      
      # train
      for (episode in 1:numEpisodes) {
        epsilon <- generateEpsilon(episode) * (!fixed) # no exploration if no training
        
        # 0. Productions takes place
        self$produce()
        
        # 1. Banks act ----
        
        # 1.0 Update Info Set
        self$getInfoSet("Banks")
        
        # 1.1 Update buffer
        if (!resetFlag & !fixed) {
          pmap(
            list(
              self$OldInfoSets$Banks, 
              bankActions,
              self$Rewards$Banks, 
              self$InfoSets$Banks
            ), 
            function(state, action, reward, next_state) {
              self$Buffer$bank$push(
                state, 
                which.max(action) - 1L, 
                reward, 
                next_state
              )
            }
          )
        }
        
        # 1.2 Make decisions
        self$DQN$bank$current$eval() # switch networks to eval mode
        
        bankActions <- lapply(
          self$InfoSets$Banks, 
          function(info) {
            decision <- self$DQN$bank$current$act(info, epsilon)
            vec <- rep(0, 27)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 1.3 Interact with economy
        self$stepBank(bankActions)
        
        # 2. Firms act ----
        
        # 2.0 Update Info Set
        self$getInfoSet("Firms")
        
        # 2.1 Update buffer
        if (!resetFlag & !fixed) {
          pmap(
            list(
              self$OldInfoSets$Firms, 
              firmActions, 
              self$Rewards$Firms, 
              self$InfoSets$Firms
            ), 
            function(state, action, reward, next_state) {
              self$Buffer$firm$push(
                state, 
                which.max(action) - 1L, 
                reward, 
                next_state
              )
            }
          )
        }
        
        # 2.2 Make decisions
        self$DQN$firm$current$eval() # switch networks to eval mode
        
        firmActions <- lapply(
          self$InfoSets$Firms, 
          function(info) {
            decision <- self$DQN$firm$current$act(info, epsilon)
            vec <- rep(0, 12)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 2.3 Interact with economy
        self$stepFirm(firmActions)
        
        # 3. Households act ----
        
        # 3.0 Update Info Set
        self$getInfoSet("Households")
        
        # 3.1 Update buffer
        if (!resetFlag & !fixed) {
          pmap(
            list(
              self$OldInfoSets$Households, 
              householdActions, 
              self$Rewards$Households, 
              self$InfoSets$Households
            ), 
            function(state, action, reward, next_state) {
              self$Buffer$household$push(
                state, 
                which.max(action) - 1L, 
                reward, 
                next_state
              ) 
            }
          )
        }
        
        # 3.2 Make decisions
        self$DQN$household$current$eval() # switch networks to eval mode
        
        householdActions <- lapply(
          self$InfoSets$Households, 
          function(info) {
            decision <- self$DQN$household$current$act(info, epsilon)
            vec <- rep(0, 13)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 3.3 Interact with economy
        self$stepHousehold(householdActions)
        
        # update history and reset if needed
        self$updateHistory(
          Decisions = list(
            bank = bankActions,
            firm = firmActions,
            household = householdActions
          ), 
          type = "episode"
        )
        
        if (runif(1) <= resetProb) {
          self$updateHistory(
            Decisions = list(
              bank = bankActions,
              firm = firmActions,
              household = householdActions
            ),
            type = "full"
          )
          self$EpisodeHistory <- list()
          self$reset()
          resetFlag = TRUE
        } else {
          resetFlag = FALSE
        }
        
        # 4. GD Step
        if (!fixed) {
          if (self$Buffer$bank$getLen() > batch_size) {
            self$DQN$bank$current$train() # switch network to train mode
            Loss$bank[episode] <- self$loss( 
              as.integer(batch_size),
              self$Buffer$bank,
              self$DQN$bank$current, 
              self$DQN$bank$target,
              self$Optimizer$bank,
              self$beta
            )$detach()$numpy()
          }
          if (self$Buffer$firm$getLen() > batch_size) {
            self$DQN$firm$current$train()
            Loss$firm[episode] <- self$loss(
              as.integer(batch_size), 
              self$Buffer$firm,
              self$DQN$firm$current, 
              self$DQN$firm$target,
              self$Optimizer$firm,
              self$beta
            )$detach()$numpy()
          }
          if (self$Buffer$household$getLen() > batch_size) {
            self$DQN$household$current$train()
            Loss$household[episode] <- self$loss( 
              as.integer(batch_size),
              self$Buffer$household,
              self$DQN$household$current, 
              self$DQN$household$target,
              self$Optimizer$household,
              self$beta
            )$detach()$numpy()
          }
          
          # 5. Update Target Network
          if (episode %% updateFreq == 0) {
            update_target(self$DQN$bank$current, self$DQN$bank$target)
            update_target(self$DQN$firm$current, self$DQN$firm$target)
            update_target(self$DQN$household$current, self$DQN$household$target)
          }
          
          self$LossData <- Loss
        }
        
        if (verbose == 1) {
          cat(".")
        } else if (verbose == 2) {
          print(self$EpisodeHistory[[length(self$EpisodeHistory)]])
        }
        
        if (!is.nan(episode %% saveEvery) & (episode %% saveEvery) == 0) {
          self$saveWeights()
          saveRDS(self, file = self$file)
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
        
        names(bankStates) <- paste(
          names(bankStates), 
          rep(
            1:length(self$Banks), 
            each = length(bankStates) / length(self$Banks)
          ), 
          sep = "_"
        )
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
        
        names(bankStates) <- paste(
          names(bankStates), 
          rep(
            1:length(self$Banks), 
            each = length(bankStates) / length(self$Banks)
          ), 
          sep = "_"
        )
        
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
        names(bankStates) <- paste(
          names(bankStates), 
          rep(
            1:length(self$Banks), 
            each = length(bankStates) / length(self$Banks)
          ), 
          sep = "_"
        )
        
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
        "output" = self$output,
        "wage" = ifelse(is.infinite(self$wage), 1e6, self$wage),
        "rate" = ifelse(is.infinite(self$rate), 1e6, self$rate)#,
        #"productivity" = self$ProductionFunction$productivity
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
      self$Queues <- self$Banks %>%
        map(function(bank) {
          queue <- bank$queue
          bank$clearQueue()
          return(queue)
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
    
    reload = function(lossFunc, bufferSize = 1000L) {
      "Resets the networks and all python objects"
      self$DQN <- self$dqnGen()$Network
      self$loadWeights()
      self$loss <- compute_td_loss
      self$createBuffer(bufferSize)
      self$createOptimizer()
      invisible(self)
    }
    ####
  )
)
