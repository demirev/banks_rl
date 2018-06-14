SimpleSavings <- R6Class(
  "A Household-Bank Saving Choice Environment",
  public = list(
    Households = list(),
    Banks      = list(),
    DQN        = list(),
    Buffer     = list(),
    Optimizer  = list(),
    loss       = NULL,
    beta       = 0.99,
    
    initialize = function(
      households, 
      banks, 
      dqns, 
      lossFunction, 
      bufferSize = 1000
    ) {
      self$Households <- households
      self$Banks      <- banks
      self$DQN        <- dqns
      self$loss       <- lossFunction
      
      Optimizer$withdraw <- optim$Adam(DQN$withdraw$current$parameters())
      Optimizer$deposit  <- optim$Adam(DQN$deposit$current$parameters())
      
      Buffer$withdraw <- ReplayBuffer(bufferSize)
      Buffer$deposit  <- ReplayBuffer(bufferSize)
      
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
          c(household$holdings, bankStates)
        }
      )
      
      return(decisionStates)
    },
    
    step = function(Withdrawals, Deposits) {
      "Steps through one turn given two decision vectors"
      
      # last element corresponds to no action (no withdrawal or deposit)
      Withdrawals <- Withdrawals[-length(Withdrawals)]
      Deposits    <- Deposits[-length(Deposits)]
      
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
          bank$interstRate
        }
      )
      lapply(
        self$Households,
        function(household) {
          household$getInterest(interests)
        }
      )
      
      NewStates <- self$getInfoSet()
      
      return(list(nextState = NewStates, reward = Consumption))
      
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
        colSums
      
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
          household$cash * (decision == 1)
          if (sum(decision) != 0) household$cash <- 0
        }
      )
      
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
    },
    
    train = function(
      numEpisodes = 10000, 
      resetProb = 0.001, 
      batch_size = 256,
      updateFreq = 200
    ) {
      # reset the economy
      InfoSets <- self$reset()
      
      # train
      for (episode in 1:numEpisodes) {
        # 0. set exploration rate
        epsilon <- generateEpsilon(episode)
        
        # 1. choose actions
        Withdrawals <- sapply(InfoSets, self$DQN$withdraw$current$act)
        Deposits    <- sapply(InfoSets, self$DQN$deposit$current$act)
        
        # 2. interact with the economy
        Episode <- self$step(Withdrawals, Deposits)
        
        # push to buffers
        pmap(
          list(InfoSets, Withdrawals, Episode$rewards, Episode$nextState), 
          function(state, action, reward, next_state) {
            self$buffer_withdraw$push(state, action, reward, next_state)
            self$buffer_deposit$push(state, action, reward, next_state)
          }
        )
        
        if (runif(1) <= resetProb) {
          InfoSets <- self$reset()
        } else {
          InfoSets <- Episode$nextState
        }
        
        # 3. GD Step
        if (episode > batch_size) {
          loss_withdraw = self$loss(
            batch_size, 
            self$DQN$withdraw$current, 
            self$DQN$withdraw$target,
            self$Optimizer$withdraw,
            self$beta
          )
          loss_deposit  = self$loss(
            batch_size, 
            self$DQN$deposit$current, 
            self$DQN$deposit$target,
            self$Optimizer$deposit,
            self$beta
          )
        }
        
        # 4. Update Target Network
        if (episode %% updateFreq == 0) {
          update_target(self$DQN$withdraw$current, self$DQN$withdraw$target)
          update_target(self$DQN$deposit$current, self$DQN$deposit$target)
        }
        
      }
    }
  )
)
