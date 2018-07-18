VanillaHousehold <- R6Class(
  "A Class for Holding Household Assets",
  public = list(
    cash      = 0,
    endowment = 0,
    income    = 0,
    holdings  = NULL,
    utilf     = NULL,
    
    initialize = function(nBanks, endowment = 100, utilf = logUtility, income = 5) {
      self$holdings <- rep(0, nBanks)
      self$cash <- self$cash + endowment
      self$endowment <- endowment
      self$income <- income
      self$utilf <- utilf
    },
    
    consume = function() {
      "Consume available cash to get utility. Get Income"
      utility <- self$utilf(self$cash)
      self$cash <- self$income
      
      return(utility)
    },
    
    getInterest = function(interests) { 
      "Receive interest on deposits"
      self$cash <- self$cash + sum(self$holdings * interests)
    }
  )
)
