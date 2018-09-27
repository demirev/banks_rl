#' A consturctor class for simulated economies
#' 
#' @section Fields:
#' 
#' \code{Households} A list of Household agents
#' 
#' \code{Banks} A list of Bank agents
#' 
#' \code{Firms} A lsit of Firm agents
#' 
#' \code{DQN} A list of deep Q networks
#' 
#' \code{Buffer} A replay buffer
#' 
#' \code{Optimizer} Optimizers for the deep q networks
#' 
#' \code{loss} A loss function for training the DQNs
#' 
#' \code{beta} Discount factor shared among all agents
#' 
#' \code{reset_hhl} A copy of the initial list of households - used when 
#' resetting
#' 
#' \code{reset_bnk} A copy of the initial list of banks - used when 
#' resetting
#' 
#' \code{reset_frm} A copy of the initial list of firms - used when 
#' resetting
#' 
#' \code{FullHistory} A list of tracked variables over simulated episodes
#' 
#' \code{EpisodeHistory} A list of tracked variables for the last episod
#' 
#' @section Methods:
#' \code{
#' $new(
#'   households = list(), 
#'   banks = list(), 
#'   firms = list(),
#'   dqns, 
#'   lossFunction, 
#'   bufferSize = 1000
#' )} Initialize the economy with a list of agents (for each type of agent), 
#' a list of neural networks, a 
#' loss function for the neural network training, buffer size for the replay
#' buffer, and a depreciation rate.
#' 
#' \code{$createOptimizer()} Create optimiers for each network.
#' 
#' \code{$createBuffer(bufferSize)} Create replay buffers for each agent.
#' 
#' \code{$step()} Step through a period of the simulation
#' 
#' \code{$updateHistory(Decisions, type)} Keep a track of the history of the
#' simulation.
#' 
#' \code{$reset()} Reset the economy to its initial state.
#' 
#' \code{$train(
#'    numEpisodes = 10000,
#'    resetProb = 0.001, 
#'    batch_size = 256,
#'    updateFreq = 200,
#'    verbose = 0,
#'    saveEvery = 0,
#'   fixed = FALSE
#' )} Train and simulate the economy. This process consists of iteratively 
#' taking decisions, acting out these decision, recording the consequences, 
#' and updating the policies.
#' 
#' @name EconomyConstructor


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

#' A Cobb Douglas production function
#' 
#' Y = A times K^{alpha} times L^{1-alpha}
#' 
#' @section Fields:
#' 
#' \code{productivity} "A" in the formula above
#' 
#' \code{capital_share} "alpha" in the formula above
#' 
#' \code{shock_mean} Mean of the shock process
#' 
#' \code{shock_sd} SD of the shock process
#' 
#' @section Methods:
#' \code{$new(capital_share = 0.3, productivity = 1)} Initialize the production 
#' function
#' 
#' \code{$shock()} Shock the productivity
#' 
#' \code{$produce(K,L)} Return "Y"
#' 
#' \code{$rate(K,L)} Return the wage
#' 
#' \code{$wage(K,L)} Return the rate on capital
#' 
#' @name CobbDouglass


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
