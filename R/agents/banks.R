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

Bank <- R6Class(
  "A Bank Class",
  public = list(
    baseDepositRate = 0,
    depositRate = 0,
    deposits = 0,
    baseLoanRate = 0,
    loanRate = 0,
    loans  = 0,
    capital = 0,
    reserves = 0,
    approvalRate = 0,
    baseApprovalRate = 0,
    defaultCounter = 0,
    capitalRatio = 0, # capital-to-loans requirement
    reserveRatio = 0, # reserves-to-loans requirement
    dividentRatio = 0,
    noDividentPeriod = 0,
    resetValues = list(),
    utilf = NULL,
    loanIncrement = 0,
    depositIncrement = 0,
    approvalIncrement = 0,
    queue = tibble(), # holds loan applications
    
    initialize = function(
      approvalRate = 1,
      depositRate = 0.09,
      deposits = 0,
      loanRate = 0.12,
      loans = 300,
      capital = 500,
      reserves = 150,
      capitalRatio = 0.08,
      reserveRatio = 0.10,
      dividentRatio = 0.03,
      noDividentPeriod = 6,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    ) {
      self$baseApprovalRate = approvalRate
      
      self$baseDepositRate = depositRate
      self$baseLoanRate = loanRate
      
      self$resetValues$deposits = deposits
      self$resetValues$loans = loans
      self$resetValues$capital = capital
      self$resetValues$reserves = reserves
      
      self$capitalRatio = capitalRatio
      self$reserveRatio = reserveRatio
      self$dividentRatio = dividentRatio
      self$noDividentPeriod = noDividentPeriod
      
      self$loanIncrement = loanIncrement
      self$depositIncrement = depositIncrement
      self$approvalIncrement = approvalIncrement
      
      self$utilf = utilf
      
      self$reset()
    },
    
    reset = function() {
      "Balance sheet to initial values"
      self$approvalRate = self$baseApprovalRate
      self$depositRate = self$baseDepositRate
      self$deposits = self$resetValues$deposits
      self$loanRate = self$baseLoanRate
      self$loans = self$resetValues$loans
      self$capital = self$resetValues$capital
      self$reserves = self$resetValues$reserves
    },
    
    getState = function() {
      "Return a vector describing the banks state for decision making"
      
      state <- c(
        "depositRate" = self$depositRate,
        "deposits" = self$deposits,
        "loanRate" = self$loanRate,
        "loans" = self$loans,
        "capital" = self$capital,
        "reserves" = self$reserves,
        "defaultCounter" = self$defaultCounter
      )
      
      return(state)
    },
    
    calculateCapital = function() {
      "Updates capital"
      assets <- self$loans + self$reserves
      liabilities <- self$deposits
      
      self$capital <- assets - liabilities
      
      invisible(self)
    },
    
    isDefault = function() {
      "Checks if bank is defaulted"
      
      self$calculateCapital()
      
      if (self$capital < 0) {
        self$reset()
        self$defaultCounter = 0
        return(1)
      } else {
        self$defaultCounter = self$defaultCounter + 1
        return(0)
      }
    },
    
    # default = function() {
    #   "Resolve a default"
    # },
    
    consume = function() {
      "Distributes part of the capital as divident"
      
      # check if bank has defaulted soon enough to be allowed to pay dividents
      if (self$defaultCounter <= self$noDividentPeriod) {
        return(self$utilf(0))
      }
      
      # check if paying divident will violate capital requirements
      if (self$capital/self$loans < self$capitalRatio) {
        return(self$utilf(0))
      } else if ((1 - self$dividentRatio)*self$capital/self$loans < self$capitalRatio) {
        divident <- self$capital - self$loans * self$capitalRatio
      } else {
        divident <- self$dividentRatio * self$capital
      }
      
      # check if paying divident will violate reserve requirements
      if (self$reserves/self$loans < self$reserveRatio) {
        return(self$utilf(0))
      } else if ((self$reserves - divident)/self$loans < self$reserveRatio) {
        divident <- self$reserves - self$loans * self$reserveRatio
      }
      
      self$capital <- self$capital - divident
      self$reserves <- self$reserves - divident
      
      utility <- self$utilf(divident)
      
      return(utility)
    },
    
    adjustLoans = function(decision) {
      "Increments the interest rate on loans up or down"
      increment <- calcIncrement(decision)
      self$loanRate <- self$loanRate + self$loanIncrement * increment
      
      if (self$loanRate <= 0.001) self$loanRate <- 0.001
      invisible(self)
    },
    
    adjustDeposits = function(decision) {
      "Increments the interest rate on deposits up or down"
      
      increment <- calcIncrement(decision)
      self$depositRate <- self$depositRate + self$depositIncrement * increment
      
      if (self$depositRate <= 0.001) self$depositRate <- 0.001
      invisible(self)
    },
    
    adjustApprovals = function(decision) {
      "Increments the cut-off FCF/Loan ratio"
      
      increment <- calcIncrement(decision)
      self$approvalRate <- self$approvalRate + self$approvalIncrement * increment 
      
      if (self$approvalRate <= 0.001) self$approvalRate <- 0.001
      invisible(self)
    },
    
    processLoan = function(application) {
      "Process a loan and return a yes/no decision"
      if (is.null(application)) return(0)
      
      # calculate net cash flow (accounting for default probability)
      ncf <- application$income * (1 - application$default_prob)^(1:application$duration)
      ncf <- sum(ncf) + application$terminal_income * (1 - application$default_prob)^application$duration 
      
      if (ncf / application$amount < self$approvalRate) {
        # loan rejected due to bank's own profitability requirements
        return(0)
      } else if (self$reserves - application$amount < self$loans * self$reserveRatio) {
        # loan rejected due to insufficient reserves
        return(0)
      } else if (self$capital/self$loans < self$capitalRatio) {
        # banks cannot issue new loans when under-capitalized
        return(0)
      } else {
        # loan approved
        self$loans <- self$loans + application$amount
        self$reserves <- self$reserves - application$amount 
        self$calculateCapital()
        return(self$loanRate)
      }
    },
    
    addQueue = function(applications) {
      "Queues received applications"
      self$queue <- applications
    },
    
    clearQueue = function() {
      "Removes everything from queue"
      self$queue <- tibble()
    },
    
    payDeposits = function() {
      "Pays out interest on deposits"
      # payment outstanding
      payment <- self$depositRate * self$deposits
      distress <- self$reserves / payment # if 1 or above, pay in full
      
      if (distress < 1) {
        payment <- self$reserves
      }
      
      self$reserves <- self$reserves - payment
      self$capital  <- self$capital - payment
      
      return(self$depositRate * min(distress, 1))
    },
    
    payWithdrawal = function(amount) { 
      "Withdrawals of depostits"
      if (amount != 0) {
        distress <- self$reserves / amount # if 1 or above, pay in full
      } else {
        distress <- 1 # bank can always repay 0
      }
      
      
      if (distress < 1) {
        amount <- self$reserves
      }
      
      self$deposits <- self$deposits - amount
      self$reserves <- self$reserves - amount
      self$calculateCapital()
      
      return(min(1, distress))
    },
    
    receivePayment = function(principal, interest) {
      "Payments on loans"
      self$loans    <- self$loans - principal
      self$reserves <- self$reserves + principal + interest
      self$capital  <- self$capital + interest
      invisible(self)
    },
   
    receiveDeposit = function(amount) {
      "New deposits"
      self$deposits <- self$deposits + amount
      self$reserves <- self$reserves + amount
      self$calculateCapital()
    }
  )
)

