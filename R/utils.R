generateEpsilon <- function(
  frame, 
  epsilon_start = 1, 
  epsilon_final = 0.001,
  epsilon_decay = 500
) {
  epsilon_final + (epsilon_start - epsilon_final) * 
    exp(-1 * frame / epsilon_decay)
}

logUtility <- function(consumptuion) {
  utility <- log(consumptuion)
  if (utility < -1e16) {
    return(-1e16) # avoid -Inf
  } else {
    return(utility)
  }
}

isoElasticUtilityGen <- function(Rho = 1) {
  
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
  if (!length(decision) %% 2) {
    stop("decision must be odd-length")
  }
  
  ind <- which(decision != 0)
  
  if (sum(ind) != 1) {
    stop("Decision must have exactly one non-zero value")
  }
  
  midpoint <- ceiling(length(decision)/2)
  
  return(ind - midpoint)
}
