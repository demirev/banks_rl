VanillaHousehold <- R6Class(
  "A Class for Holding Household Assets",
  public = list(
    cash      = 0,
    endowment = 0,
    holdings  = NULL,
    utilf     = NULL,
    
    initialize = function(nBanks, endowment = 100, utilf = function(c) log(c)) {
      self$holdings <- rep(0, nBanks)
      self$cash <- self$cash + endowment
      self$endowment <- endowment
      self$utilf <- utilf
    },
    
    consume = function() {
      "Consume available cash to get utility"
      
      utility <- self$utilf(self$cash)
      self$cash <- 0
      
      return(utility)
    },
    
    getInterest = function(interests) {
      "Receive interest on deposits"
      self$holdings <- self$holdings + self$hodlings * interests
    }
  )
)
