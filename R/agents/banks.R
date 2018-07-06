DummyBank <- R6Class(
  "A Dummy Bank Class for Testing Households",
  public = list(
    interestRate = 0,
    deposits = 0,
    loans  = 0,
    capital = 0,
    reserves = 0,
    provisions = 0,
    rating = 1,
    defaultCounter = 0,
    
    initialize = function(
      interestRate = 0.04,
      deposits = 0,
      loans = 300,
      capital = 500,
      reserves = 150,
      provisions = 50,
      rating = 3
    ) {
      self$interestRate = interestRate
      self$deposits = deposits
      self$loans = loans
      self$capital = capital
      self$reserves = reserves
      self$provisions = provisions
      self$rating = rating
    },
    
    getState = function() {
      "Return a vector describing the banks state for decision making"
      
      state <- c(
        self$interestRate,
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
      
      loanReturns <- self$loans * rnorm(1, mean = 0.01 - 0.005*self$rating, sd = 0.01*self$rating)
      
      self$loans <- self$loans + loanReturns
      if (loanReturns > 0) self$reserves <- self$reserves + loanReturns
      self$capital <- self$reserves + self$loans + self$provisions - self$deposits
      
      # check if bank has become insolvent
      if (self$capital < 0) {
        
        outcome <- 1 # bank fails
        
        # restart values
        self$deposits = 0
        self$loans = 300
        self$capital = 500
        self$reserves = 150
        self$provisions = 50
        self$defaultCounter = 0
        
        return(outcome)
      } else {
        outcome <- 0
        self$defaultCounter = self$defaultCounter + 1
      }
      
      self$interestRate <- max(0.09 + 0.01*self$rating + rnorm(1,0,0.001*self$rating), 0.001)
      
      newLoans <- self$reserves * abs(rnorm(1, mean = 0.015 + 0.005*self$rating, sd = 0.01*self$rating))
      self$reserves <- self$reserves - newLoans
      self$loans <- self$loans + newLoans
      
      return(outcome)
      
    }
  )
)
