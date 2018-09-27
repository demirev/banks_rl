#' A Household agent
#' 
#' Implements a household in the ABM.
#'
#' @section Fields:
#' \code{cash} The amount of cash (consumption good) held by the household. The
#' household cannot keep this cash for more than one period - it must be either
#' consumed or deposited.
#' 
#' \code{labor} The amount of labor owned by the household. Stays fixed.
#' 
#' \code{holdings} A vector of the same size as the number of banks in the 
#' simulation, representing the amount the household has deposited in each
#' of the banks
#' 
#' \code{utilf} The utility function used to convert cash to utils
#' 
#' @section Methods:
#' \code{
#' $new(
#'   nBanks,
#'   endowment = 100,
#'   utilf = logUtility,
#'   labor = 1
#' )} Initialize the household with a given endowment of the consumption good 
#' and labor 
#' 
#' \code{$consume()} Consume all cash and receive utility
#' 
#' \code{$getState()} Return a named vector summarizing the state of the 
#' household (cash and holdings)
#' 
#' \code{$receiveInterest(interests)} Given the current intersts on deposits for
#' each bank, increase cash by the interest amount.
#' 
#' \code{$receiveWage(wage)} Given the current wage increase cash by the 
#' households labor times the wage.
#' 
#' \code{$requesstWithdrawal(decision)} Request a deposit withdrawal from a
#' given bank. Returns the amounts to be withdrawn, which are then send to 
#' the banks. The bank may not be able to return the deposit if doing so will
#' violate some of its regulatory requirements.
#' 
#' \code{$receiveWithdrawal(decision, distress)} After requested withdrawal from
#' some of the banks (stored in the decision variable), the bank  processes the
#' request and either pays back in full (if doing so wouldn't violate any
#' regulatory requirements) or in part (only up to the amount they can allow
#' without violating the regulations). This is passed through the distress 
#' variable. Households cash is increased accordingly
#' 
#' \code{$deposit(decision)} Deposit all cash on hand to a given bank (given by
#' the decision vector)
#' 
#' \code{$bankDefaults(outcomes)} Given a vector of bank defaults (outcomes),
#' remove all deposits held in defaulted banks
#' 
#' @name Household

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
