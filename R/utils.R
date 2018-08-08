generateEpsilon <- function(
  frame, 
  epsilon_start = 1, 
  epsilon_final = 0.005,
  epsilon_decay = 5000
) {
  "Generate an exploration rate"
  epsilon_final + (epsilon_start - epsilon_final) * 
    exp(-1 * frame / epsilon_decay)
}

logUtility <- function(consumptuion) {
  "Logarithmic utility"
  utility <- log(consumptuion)
  if (utility < -1e16) {
    return(-1e16) # avoid -Inf
  } else {
    return(utility)
  }
}

isoElasticUtilityGen <- function(Rho = 1) {
  "A utility function genrator"
  utilF <- function(consumptuion, rho = Rho) {
    
    if (rho == 1) {
      utility <- log(consumptuion)
    } else {
      utility <- (consumptuion^(1 - rho) - 1) / (1 - rho)
    }
    
    if (utility < -1e16) {
      return(-1e16) # avoid -Inf
    } else {
      return(utility)
    }
  }
  
  return(utilF)
  
}

calcIncrement <- function(decision) {
  "Converts and odd-one-hot vector to a number between -floor(length(vector)/2) and +floor(length(vector)/2)"
  if (!length(decision) %% 2) {
    stop("decision must be odd-length")
  }
  
  ind <- which(decision != 0)
  
  if (sum(decision != 0) != 1) {
    stop("Decision must have exactly one non-zero value") 
  }
  
  midpoint <- ceiling(length(decision)/2)
  
  return(ind - midpoint)
}

CobbDouglass <-  R6Class(
  "A Cobb-Douglass Production Function",
  public = list(
    productivity = 1,
    capital_share = NULL,
    shock_mean = 0,
    shock_sd   = 0,
    
    initialize = function(capital_share = 0.3) {
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
