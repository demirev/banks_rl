DummyBank <- R6Class(
  "A Dummy Bank Class for Testing Households",
  public = list(
    baseDepositRate = 0,
    depositRate = 0,
    deposits = 0,
    loans  = 0,
    capital = 0,
    reserves = 0,
    provisions = 0,
    rating = 1,
    defaultCounter = 0,
    fixedInterest = FALSE,
    resetValues = list(),
    
    initialize = function(
      depositRate = 0.09,
      deposits = 0,
      loans = 300,
      capital = 500,
      reserves = 150,
      provisions = 50,
      rating = 3,
      fixedInterest = FALSE
    ) {
      self$baseDepositRate = depositRate
      self$rating = rating
      self$fixedInterest = fixedInterest
      
      self$resetValues$deposits = deposits
      self$resetValues$loans = loans
      self$resetValues$capital = capital
      self$resetValues$reserves = reserves
      self$resetValues$provisions = provisions
      
      self$reset()
    },
    
    reset = function() {
      "Balance sheet to initial values"
      self$depositRate = self$baseDepositRate
      self$deposits = self$resetValues$deposits
      self$loans = self$resetValues$loans
      self$capital = self$resetValues$capital
      self$reserves = self$resetValues$reserves
      self$provisions = self$resetValues$provisions
    },
    
    getState = function() {
      "Return a vector describing the banks state for decision making"
      
      state <- c(
        self$depositRate,
        self$deposits,
        self$loans,
        self$capital,
        self$reserves,
        self$provisions,
        self$defaultCounter
      )
        
      return(state)
    },
    
    act = function() {
      "Dummy bank decisions" 
      
      loanReturns <- self$loans * rnorm(1, mean = 0.001 - 0.0005*self$rating, sd = 0.001*self$rating)
      
      self$loans <- self$loans + loanReturns
      if (loanReturns > 0) self$reserves <- self$reserves + loanReturns
      self$capital <- self$reserves + self$loans + self$provisions - self$deposits
      
      # check if bank has become insolvent
      if (self$capital < 0 | self$reserves < 0) {
        
        outcome <- 1 # bank fails
        
        # restart values
        self$reset()
        self$defaultCounter = 0
        
        return(outcome)
      } else {
        outcome <- 0
        self$defaultCounter = self$defaultCounter + 1
      }
      
      if (!self$fixedInterest) {
        self$depositRate <- max(
          self$baseDepositRate + 0.01*self$rating + rnorm(1,0,0.001*self$rating), 
          0.001
        )
      }
      
      newLoans <- self$reserves * abs(rnorm(1, mean = 0.015 + 0.005*self$rating, sd = 0.01*self$rating))
      self$reserves <- self$reserves - newLoans
      self$loans <- self$loans + newLoans
      
      return(outcome)
      
    }
  )
)


DummyBankInv <- R6Class(
  "A Dummy Bank Class for Testing Firms",
  inherit = DummyBank,
  public = list(
    baseLoanRate = 0,
    loanRate = 0,
    
    initialize = function(
      loanRate = 0.1,
      depositRate = 0.09,
      deposits = 1000,
      loans = 0,
      capital = 4000,
      reserves = 5000,
      provisions = 0,
      rating = 3,
      fixedInterest = FALSE
    ) {
      self$baseLoanRate = loanRate
      self$baseDepositRate = depositRate
      self$rating = rating
      self$fixedInterest = fixedInterest
      
      self$resetValues$deposits = deposits
      self$resetValues$loans = loans
      self$resetValues$capital = capital
      self$resetValues$reserves = reserves
      self$resetValues$provisions = provisions
      
      self$reset()
    },
    
    reset = function() {
      super$reset()
      self$loanRate = self$baseLoanRate
    },
    
    act = function() {
      "Dummy bank decisions" 
      # repay interest
      depositPayments <- self$deposits * self$depositRate
      self$reserves <- self$reserves - depositPayments
      
      # receive new deposits (inflow or outflow)
      newDeposits <- self$deposits * rnorm(1, mean = 0.025 - 0.005*self$rating, sd = 0.01*self$rating)
      if (newDeposits < -self$deposits) newDeposits <- -self$deposits
      
      self$reserves <- self$reserves + newDeposits
      self$deposits <- self$deposits + newDeposits
      
      self$capital <- self$reserves + self$loans + self$provisions - self$deposits
      
      # check if bank has become insolvent
      if (self$capital < 0 | self$reserves < 0) {
        
        outcome <- 1 # bank fails
        
        # restart values
        self$reset()
        self$defaultCounter = 0
        
        return(outcome)
      } else {
        outcome <- 0
        self$defaultCounter = self$defaultCounter + 1
      }
      
      if (!self$fixedInterest) {
        self$depositRate <- max(
          self$baseDepositRate + 0.01*self$rating + rnorm(1,0,0.001*self$rating), 
          0.001
        )
        self$loanRate <- max(
          self$baseLoanRate - 0.01*self$rating + rnorm(1,0,0.01*self$rating), 
          0.001
        )
      }
      
      return(outcome)
      
    },
    
    process = function(application) {
      "Rejects or approves a loan application"
      
      if (is.null(application)) return(0)
      
      npv <- application$income * (1 - application$default_prob)^(1:application$duration)
      npv <- sum(npv) + application$terminal_income * (1 - application$default_prob)^application$duration
      
      if (npv / application$amount >= 0.2 - 0.05*self$rating & self$reserves >= application$amount) {
        self$loans <- self$loans + application$amount
        self$reserves <- self$reserves - application$amount 
        return(self$loanRate)
      } else {
        return(0)
      }
      
    },
    
    getState = function() {
      "Return a vector describing the banks state for decision making"
      
      state <- super$getState()
      state <- c(state, self$loanRate)
      
      return(state)
    }
  )
)
