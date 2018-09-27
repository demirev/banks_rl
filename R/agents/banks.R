#' A Bank agent
#' 
#' Implements a bank in the ABM.
#'
#' @section Fields:
#' \code{baseDepositRate} A numeric value for the initial deposit rate of the 
#' bank to be used when initiating the simulation/training. This is also the 
#' deposit interest rate to which the bank reverts if it defaults.
#' 
#' \code{depositRate} The current deposit rate based on which deposit payments
#' to the households are calculated. The bank can change this rate in increments
#' defined by `depositIncrement`. All deposits at the bank pay out this rate 
#' (regardless of the rate at the time of making the deposits)
#' 
#' \code{deposits} The total amount of deposits held at the bank at this moment
#' 
#' \code{baseLoanRate} A numeric value for the initial loan rate of the 
#' bank to be used when initiating the simulation/training. This is also the 
#' loan interest rate to which the bank reverts if it defaults.
#' 
#' \code{loanRate} The current loan rate based on which loan reapyments
#' on newly issued loans are calculated. The bank can change this rate in 
#' increments defined by `loanIncrement`. Once issued, a loan will have the 
#' same interest rate until termination
#' 
#' \code{loans} The total amount of loans outstanding (principals only)
#' 
#' \code{capital} The banks capital - it is defined as total assets minus 
#' non-equity liabilities. I.e. Reserves + loans - deposits
#' 
#' \code{reserves} The amount of reserves held at the bank. Measured in terms
#' of the consumption good (or cash).
#' 
#' \code{approvalRate} Every time the bank considers a loan for approval, it
#' measures the expected future streams of income from the project (in terms
#' of the capital good) and compare it to the laon amount (in terms of cash).
#' If this income-to-loan ratio is higher than approvalRate the bank approves
#' the loan. The approvalRate is a control variable that the bank can increment
#' similar to the interest rates.
#'  
#' \code{baseApprovalRate} The approval rate when the simulation is initialized
#' and to which the bank reverts when defaulting.
#' 
#' \code{defaultCounter} The number of periods since the bank last defaulted.
#' 
#' \code{capitalRatio} The required capital adequacy ratio. The bank is not 
#' allowed to pay out interest or give loans or pay out dividents if it 
#' brings it below this ratio.
#' 
#' \code{reserveRatio} The required reserve-to-deposit ratio. The bank is not 
#' allowed to pay out interest or dividents or give loans if doing so will 
#' bring it below this ratio.
#' 
#' \code{dividentRatio} The portion of capital paid out as dividents (if doing
#' so will not make the bank violate the capital or reserve requirements)
#' 
#' \code{noDividentPeriod} The number of period after default in which the bank
#' is not allowed to pay out dividents
#' 
#' \code{resetValues} A list of balance values (reserves, capital, etc) to be 
#' used when the bank resets at default
#' 
#' \code{utilf} The utility function of the banks management. Converts dividents
#' to utils.
#' 
#' \code{loanIncrement} The increment with which the bank can move the loan rate
#' up or down.
#' 
#' \code{depositIncrement} The increment with which the bank can move the 
#' deposit rate up or down.
#' 
#' \code{approvalIncrement} The incement with which the bank can move the 
#' approval rate up or down.
#' 
#' \code{queue} A tibble holding the loan details of all application received
#' by the bank for this period.
#' 
#' @section Methods:
#' \code{
#' $new(
#'   approvalRate = 1,
#'   depositRate = 0.09,
#'   deposits = 0,
#'   loanRate = 0.12,
#'   loans = 0,
#'   capital = 500,
#'   reserves = 500,
#'   capitalRatio = 0.08,
#'   reserveRatio = 0.10,
#'   dividentRatio = 0.03,
#'   noDividentPeriod = 6,
#'   utilf = logUtility,
#'   loanIncrement = 0.0025,
#'   depositIncrement = 0.0025,
#'   approvalIncrement = 0.0025
#' )} Initialize the bank class and set initial field values. The initial 
#' balance amounts will be saved in `resetValues` and used in case the bank
#' defaults.
#' 
#' \code{$getState()} Returns a named vector summarizing the bank's state (
#' balance sheet, interest rates, and a summary of outstanding loans) to be used
#' in decision making of all agents.
#' 
#' \code{$isDefault()} Checks whether the bank has negative capital (deposits
#' exceeding reserves and loans)
#' 
#' \code{$consume()} Pay out divident and receive utility
#' 
#' \code{$ajdustLoans(decision)} Taking a decision (a one-hot vector of size 3)
#' adjust the loan interest rate up (if decision[3]==1), down (if decision[1]
#' ==1), or not at all (if decision[2]==1)
#' 
#' \code{$adjustDeposits(decision)} Taking a decision (a one-hot vector of size
#' 3) adjust the deposit interest rate up (if decision[3]==1), down (if 
#' decision[1]==1), or not at all (if decision[2]==1)
#' 
#' \code{$adjustApprovals(decision)} Taking a decision (a one-hot vector of size
#' 3) adjust the approval rate up (if decision[3]==1), down (if 
#' decision[1]==1), or not at all (if decision[2]==1)
#' 
#' \code{$processLoan(application)} Given a loan application, caluclate the
#' income-to-laon reatio, compare it to the threshold approval rate and either
#' reject the application or issue the loan, updating the bank's balance.
#' 
#' \code{$addQueue(applications)} Update the bank's loan queue
#' 
#' \code{$clearQueue()} Clear the bank's loan queue
#' 
#' \code{$payDeposits()} Pay out deposit interest to households.
#' 
#' \code{$payWithdrawal(amount)} Reduce deposits (and reserves) based on 
#' households decision to withdraw loans
#' 
#' \code{$receivePayment(principal, interest)} Process a repayment by a firm on
#' an outstanding loan. Each payment consists of a principal (which decreases
#' the loan amount on books) and an interest (which increases capital). Reserves
#' are also increased bu principal+interest
#' 
#' \code{$receiveDeposit(amount)} Take a new deposit, increase reserves and 
#' on-book deposits.
#'
#' @name Bank

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
      loans = 0,
      capital = 500,
      reserves = 500,
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
        "approvalRate" = self$approvalRate,
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
    
    consume = function() {
      "Distributes part of the capital as divident"
      
      # check if bank has defaulted soon enough to be allowed to pay dividents
      if (self$defaultCounter <= self$noDividentPeriod) {
        return(self$utilf(0))
      }
      
      # check if paying divident will violate capital requirements
      if (self$loans != 0 & self$capital/self$loans < self$capitalRatio) {
        return(self$utilf(0))
      } else if (self$loans != 0 & (1 - self$dividentRatio)*self$capital/self$loans < self$capitalRatio) {
        divident <- self$capital - self$loans * self$capitalRatio
      } else {
        divident <- self$dividentRatio * self$capital
      }
      
      # check if paying divident will violate reserve requirements
      if (self$loans != 0 & self$reserves/self$loans < self$reserveRatio) {
        return(self$utilf(0))
      } else if (self$loans != 0 & (self$reserves - divident)/self$loans < self$reserveRatio) {
        divident <- self$reserves - self$loans * self$reserveRatio
      }
      
      self$capital <- self$capital - divident
      self$reserves <- self$reserves - divident
      
      utility <- self$utilf(divident)
      
      return(utility)
    },
    
    adjustLoans = function(decision) {
      "Increments the interest rate on loans up or down"
      self$loanRate <- self$loanRate + self$loanIncrement * decision
      
      if (self$loanRate <= 0.001) self$loanRate <- 0.001
      invisible(self)
    },
    
    adjustDeposits = function(decision) {
      "Increments the interest rate on deposits up or down"
      self$depositRate <- self$depositRate + self$depositIncrement * decision
      
      if (self$depositRate <= 0.001) self$depositRate <- 0.001
      invisible(self)
    },
    
    adjustApprovals = function(decision) {
      "Increments the cut-off FCF/Loan ratio"
      self$approvalRate <- self$approvalRate + self$approvalIncrement * decision
      
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
      } else if ((self$reserves - application$amount) < (self$loans * self$reserveRatio)) {
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
      distress <- ifelse(payment == 0, 1, self$reserves / payment) # if 1 or above, pay in full
      
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

