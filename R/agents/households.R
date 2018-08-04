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

Household <- R6Class(
  "A Household Class for the intermediation environment",
  public = list(
    cash      = 0,
    labor     = NULL,
    holdings  = NULL,
    utilf     = NULL,
    
    initialize = function(nBanks, endowment = 100, utilf = logUtility, labor = 1) {
      self$holdings <- rep(0, nBanks)
      self$cash <- self$cash + endowment
      self$utilf <- utilf
      self$labor <- labor
    },
    
    consume = function() {
      "Consume available cash to get utility. Get Income"
      utility <- self$utilf(self$cash)
      self$cash <- 0
      
      return(utility)
    },
    
    getState = function() {
      depositStates <- self$holdings
      names(depositStates) <- paste0("Deposit_", 1:length(depositStates))
      return(c("cash" = self$cash, depositStates))
    },
    
    receiveInterest = function(interests) { 
      "Receive interest on deposits"
      self$cash <- self$cash + sum(self$holdings * interests)
      invisible(self)
    },
    
    receiveWage = function(wage) {
      self$cash <- self$cash + self$labor * wage
      invisible(self)
    },
    
    requestWithdrawal = function(decision) { 
      # vector with amount withdrawn from each bank
      amount <- self$holdings * (decision == 1)
      
      return(amount)
    },
    
    receiveWithdrawal = function(decision, distress) {
      # vector with amount withdrawn from each bank
      amount <- self$holdings * (decision == 1) * distress
      self$cash <- self$cash + sum(amount)
      
      # set withdrawals to zero
      self$holdings[decision == 1] <- 0
    },
    
    deposit = function(decision) { 
      # vector with amount deposited in each bank
      res <- self$cash * (decision == 1)
      self$holdings <- self$holdings + res
      if (sum(decision) != 0) self$cash <- 0
      return(res)
    },
    
    bankDefaults = function(outcomes) {
      self$holdings[outcomes] <- 0 # loses savings in defaulted banks
    }
    
  )
)
