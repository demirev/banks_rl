EconomyConstructor <- R6Class(
  "Methods and fields shared among classes",
  public = list(
    Households     = list(),
    Banks          = list(),
    Firms          = list(),
    DQN            = list(),
    Buffer         = list(),
    Optimizer      = list(),
    loss           = NULL,
    beta           = 0.99,
    reset_hhl      = list(),
    reset_bnk      = list(),
    reset_frm      = list(),
    FullHistory    = list(),
    EpisodeHistory = list(),
    
    initialize = function(
      households = list(), 
      banks = list(), 
      firms = list(),
      dqns, 
      lossFunction, 
      bufferSize = 1000
    ) {
      self$Households <- households
      self$Banks      <- banks
      self$Firms      <- firms
      
      self$DQN        <- dqns
      self$loss       <- lossFunction
      
      self$reset_hhl  <- lapply(households, function(x) x$clone())
      self$reset_bnk  <- lapply(banks, function(x) x$clone())
      self$reset_frm  <- lapply(firms, function(x) x$clone())
      
      self$createOptimizer()
      self$createBuffer(bufferSize)
      
    },
    
    createOptimizer = function() {
      "Initialize the optimizers"
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      invisible(self)
    },
    
    step = function(decisions) { 
      "Steps through one turn given decision vectors"
      invisible(self)
    },
    
    updateHistory = function(type = "episode") {
      "Saves varaibles"
      invisible(self)
    },
    
    reset = function() {
      self$Households <- self$reset_hhl %>%
        sample(length(self$reset_hhl)) %>%
        lapply(function(x) x$clone())
      self$Banks <- self$reset_bnk  %>%
        sample(length(self$reset_bnk)) %>%
        lapply(function(x) x$clone())
      self$Firms <- self$reset_frm  %>%
        sample(length(self$reset_frm)) %>%
        lapply(function(x) x$clone())
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
      invisible(self)
    }
    
  )
)

CobbDouglass <- R6Class(
  "A Cobb-Douglass Production Function",
  public = list(
    productivity = NULL,
    capital_share = NULL,
    shock_mean = 0,
    shock_sd   = 0,
    
    initialize = function(capital_share = 0.3, productivity = 1) {
      self$productivity <- productivity
      self$capital_share <- capital_share
    },
    
    shock = function() {
      self$productivity <- self$productivity * rlnorm(1, self$shock_mean, self$shock_sd)
    },
    
    produce = function(K, L) { 
      return(self$productivity * K^self$capital_share * L^(1 - self$capital_share))
    },
    
    rate = function(K, L) {
      return(self$capital_share * self$productivity * K^(self$capital_share - 1) * L^(1 - self$capital_share))
    },
    
    wage = function(K, L) {
      return((1 - self$capital_share) * self$productivity * K^self$capital_share * L^(-self$capital_share))
    }
  )
)
