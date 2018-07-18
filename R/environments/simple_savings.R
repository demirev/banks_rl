SimpleSavings <- R6Class(
  "A Household-Bank Saving Choice Environment",
  inherit = EconomyConstructor,
  public = list(
    
    createOptimizer = function() {
      "Initialize the optimizers"
      self$Optimizer$withdraw <- optim$Adam(self$DQN$withdraw$current$parameters())
      self$Optimizer$deposit  <- optim$Adam(self$DQN$deposit$current$parameters())
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      self$Buffer$withdraw <- ReplayBuffer(bufferSize)
      self$Buffer$deposit  <- ReplayBuffer(bufferSize)
      invisible(self)
    },
    
    step = function(Withdrawals, Deposits) { 
      "Steps through one turn given two decision vectors"
      
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
      
      # 4. Banks change their interests or fail
      bankOutcomes <- lapply(
        self$Banks,
        function(bank) {
          bank$act() # dummy actions by each bank
        }
      )
      
      self$defaults(bankOutcomes)
      
      # 5. Interest is received
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
      
      NewStates <- self$getInfoSet()
      
      self$updateHistory("episode")
      
      return(list(nextState = NewStates, reward = Consumption))
      
    },
    
    updateHistory = function(type = "episode") {
      # all deposits for history
      allDeposits <- lapply(
        self$Households,
        function(household) {
          household$holdings
        }
      ) %>%
        reduce(cbind)
      
      # all cash
      allCash <- lapply(
        self$Households, 
        function(household) {
          household$cash
        }
      ) %>%
        reduce(cbind)
      
      
      # bank details
      bankBalances <- lapply(
        self$Banks,
        function(bank) {
          tibble(
            type = bank$rating,
            depositRate = bank$depositRate,
            #loanRate = bank$loanRate,
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
          deposits = allDeposits,
          cash     = allCash,
          banks    = bankBalances
        )
      } else if (type == "full") {
        # add current state to full history
        self$FullHistory[[length(self$FullHistory) + 1]] <- self$EpsiodeHistory
      }
      
      invisible(self)
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
        withdraw = rep(NA, numEpisodes), 
        deposit = rep(NA, numEpisodes)
      )
      
      InfoSets <- self$reset()$getInfoSet()
      
      # train
      for (episode in 1:numEpisodes) {
        # 0. set exploration rate
        epsilon <- generateEpsilon(episode)
        
        # 1. choose actions
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
        
        # 2. interact with the economy
        Episode <- self$step(Withdrawals, Deposits)
        
        # push to buffers
        pmap(
          list(InfoSets, Withdrawals, Deposits, Episode$reward, Episode$nextState), 
          function(state, action_w, action_d, reward, next_state) {
            self$Buffer$withdraw$push(state, which.max(action_w) - 1L, reward, next_state)
            self$Buffer$deposit$push(state, which.max(action_d) - 1L, reward, next_state)
          }
        )
        
        if (runif(1) <= resetProb) {
          InfoSets <- self$reset()$getInfoSet()
          self$updateHistory("full")
          self$EpisodeHistory <- list()
        } else {
          InfoSets <- Episode$nextState
        }
        
        # 3. GD Step
        if (self$Buffer$withdraw$getLen() > batch_size) {
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
        }
        
        # 4. Update Target Network
        if (episode %% updateFreq == 0) {
          update_target(self$DQN$withdraw$current, self$DQN$withdraw$target)
          update_target(self$DQN$deposit$current, self$DQN$deposit$target)
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
      
      # Combine it with the households' holdings
      decisionStates <- lapply(
        self$Households,
        function(household) {
          c(household$cash, household$holdings, bankStates)
        }
      )
      
      return(decisionStates)
    },
    
    withdraw = function(Decisions) {
      "Decreases bank balances given a list of per household withdrawals" 
      amounts <- map2(
        self$Households, Decisions,
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
        self$Banks, amounts,
        function(bank, amount){
          bank$deposits <- bank$deposits - amount
          bank$reserves <- bank$reserves - amount
        }
      )

    },
    
    deposit = function(Decisions) {
      "Increases bank balances given a list of per household withdrawals" 
      amounts <- map2(
        self$Households, Decisions,
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
        self$Banks, amounts,
        function(bank, amount){
          bank$deposits <- bank$deposits + amount
          bank$reserves <- bank$reserves + amount
        }
      )
      
    },
    
    defaults = function(bankOutcomes) {
      "Decreases households outstanding deposits given a list of defaulted banks"
      lapply(
        self$Households,
        function(household) {
          household$holdings[bankOutcomes == 1] <- 0 # loses savings
        }
      )
    }
    
  )
)
