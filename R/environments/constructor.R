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
      self$Households <- lapply(self$reset_hhl, function(x) x$clone())
      self$Banks <- lapply(self$reset_bnk, function(x) x$clone())
      self$Firms <- lapply(self$reset_frm, function(x) x$clone())
      invisible(self)
    },
    
    train = function(
      numEpisodes = 10000,
      resetProb = 0.001, 
      batch_size = 256,
      updateFreq = 200
    ) {
      "Train the networks"
      invisible(self)
    }
    
  )
)
