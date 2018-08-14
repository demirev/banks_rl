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

getChanges <- function(bankActions, n = 1) {
  "Convert a bank action index to three vectors of price changes"
  grid <- expand.grid(
    loanChange = -n:n, 
    depositChange = -n:n, 
    approveChange = -n:n
  )
  bankActions %>% 
    map(function(action){
      list(
        loanChange = grid$loanChange[which(action == 1)], 
        depositChange = grid$depositChange[which(action == 1)], 
        approveChange = grid$approveChange[which(action == 1)]
      )
    })
}

CobbDouglass <- R6Class(
  "A Cobb-Douglass Production Function",
  public = list(
    productivity = NULL,
    capital_share = NULL,
    shock_mean = 0,
    shock_sd   = 0,
    
    initialize = function(capital_share = 0.3, productivity = 1) {
      self$productivity <- productivity
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

findPropensity <- function(net, x, variable, vrange, vlength = 400, ind = 1) {
  "Find how Q values dchange with some input"
  net$eval()
  toTorch <- function(x) Variable(torch$FloatTensor(np$float32(t(as.matrix(x)))))
  seq(vrange[1], vrange[2], length.out = vlength) %>%
    map(
      function(v) {
        x[variable] <- v
        net$forward(toTorch(x))$detach()$numpy()[, ind]
      }
    )
} 

findRecessions <- function(output, streak = 2) {
  "Identify recession starts and ends"
  outputDecrease = (diff(output) < 0)
  outputIncrease = (diff(output) > 0)
  
  decreases <- 0:(streak - 1) %>%
    map(function(s) {
      lag(outputDecrease, s)
    }) %>%
    reduce(cbind) %>%
    rowSums
  decreases <- which(decreases == streak)
  
  increases <- 0:(streak - 1) %>%
    map(function(s) {
      lag(outputIncrease, s)
    }) %>%
    reduce(cbind) %>%
    rowSums
  increases <- which(increases == streak)
  
  endPoints <- decreases %>%
    map(function(decrease) {
      firstIncrease <- increases[increases > decrease][1]
      if (is.na(firstIncrease)) firstIncrease <- length(output)
      firstIncrease
    }) %>%
    reduce(c)
  
  tibble(
    start = decreases,
    end = endPoints
  ) %>%
    filter(!duplicated(end)) %>%
    mutate(duration = end - start)
}

countWithdrawals <- function(History, zeroToNa = FALSE) {
  "Count withdrawals from a certain bank"
  counts <- History %>% 
    map("decisions") %>% 
    map("household") %>%
    map(function(x) {
      x %>%
        map(function(y) (y[1] != 1) & (y[2] != 1)) %>%
        reduce(sum)
    }) %>%
    reduce(c)
  
  if (zeroToNa) counts[counts == 0] <- NA
  counts
}

countDefaults <- function(History, zeroToNa = TRUE) {
  "Count the defaults of a certain bank"
  counts <- History %>%
    map("banks") %>%
    map("defaultCounter") %>%
    map(function(x) sum(x == 0)) %>%
    reduce(c)
  
  if (zeroToNa) counts[counts == 0] <- NA
  counts
}

deriveTimeSeries <- function(History, reverseUtil = exp) {
  "Take a history object and derive some key economic time series"
  output <- History %>%
    map("macro") %>%
    map("output") %>%
    reduce(c)
  
  capital <- History %>%
    map("macro") %>%
    map("capital") %>%
    reduce(c)
  
  wage <- History %>%
    map("macro") %>%
    map("wage") %>%
    reduce(c)
  
  rate <- History %>%
    map("macro") %>%
    map("rate") %>%
    reduce(c)
  
  depositRate <- History %>%
    map("banks") %>%
    map("depositRate") %>%
    map(mean) %>%
    reduce(c)
  
  loanRate <- History %>%
    map("banks") %>%
    map("loanRate") %>%
    map(mean) %>%
    reduce(c)
  
  approvalRate <- History %>%
    map("banks") %>%
    map("approvalRate") %>%
    map(mean) %>%
    reduce(c)
  
  loans <- History %>%
    map("banks") %>%
    map("loans") %>%
    map(sum) %>%
    reduce(c)
  
  deposits <- History %>%
    map("banks") %>%
    map("deposits") %>%
    map(sum) %>%
    reduce(c)
  
  numberWithdrawals <- countWithdrawals(History,FALSE)
  
  consumption <- History %>%
    map("rewards") %>%
    map(function(periodRewards) {
      periodRewards %>%
        map(function(agentRewards){
          agentRewards %>%
            map(reverseUtil) %>%
            reduce(sum)
        }) %>%
        reduce(sum)
    }) %>%
    reduce(c)
  
  allInvestment <- History %>%
    map("projects") %>%
    reduce(rbind) %>%
    distinct(bank, amount, income, income_sd, terminal_income, 
             terminal_income_sd, liquidation, .keep_all = T) %>%
    select(c(bank, amount, income, income_sd, terminal_income, 
             terminal_income_sd, liquidation, duration))
  
  investment <- History %>%
    map("projects") %>%
    map(function(tb) {
      tb %>%
        inner_join(
          allInvestment, 
          by = c("bank", "amount", "income", "income_sd", "terminal_income", 
                 "terminal_income_sd", "liquidation", "duration")
        ) %>% 
        select(amount) %>%
        sum
    }) %>%
    reduce(c)
  
  return(
    tibble(
      output,
      capital,
      wage,
      rate,
      depositRate,
      loanRate,
      approvalRate,
      deposits,
      loans,
      numberWithdrawals,
      consumption,
      investment
    )
  )
}

