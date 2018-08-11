source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/intermediation.R")
source_python("python/dqn.py")

# Test B1 ------------------------------------------------------------------
# 10 banks
Banks_B1 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.09,
      deposits = 0,
      loanRate = 0.12,
      loans = 0,
      capital = 5000,
      reserves = 5000,
      capitalRatio = 0.08,
      reserveRatio = 0.10,
      dividentRatio = 0.03,
      noDividentPeriod = 6,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

# 100 firms
Firms_B1 <- lapply(
  1:100,
  function(i) {
    Firm$new(
      nBanks, 
      endowment = 100, # initial cash
      endowment_k = 1, # initial capital
      utilf = logUtility, 
      Pool = ProjectPool$new(
        duration = c(6,20),
        amount   = c(10, 20),
        income   = c(0, 10),
        income_sd = c(0, 1),
        terminal_income = c(0, 8),
        terminal_income_sd = c(0, 1),
        default_prob = c(0, .03),
        liquidation = c(0, 0.6) # as multiple of amount
      )
    )
  }
)

# 500 households
Households_B1 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_B1),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

DQN_B1 <- function() {
  Network <- list(
    invest = list(
      current = DQN(132L, 2L), #$cuda(),
      target  = DQN(132L, 2L)
    ),
    borrow = list(
      current = DQN(132L, 11L),
      target  = DQN(132L, 11L)
    ),
    deposit = list(
      current = DQN(94L, 11L),
      target  = DQN(94L, 11L)
    ),
    withdraw = list(
      current = DQN(94L, 11L),
      target  = DQN(94L, 11L)
    ),
    loanrate = list(
      current = DQN(91L, 5L),
      target  = DQN(91L, 5L)
    ),
    depositrate = list(
      current = DQN(91L, 5L),
      target  = DQN(91L, 5L)
    ),
    approverate = list(
      current = DQN(91L, 5L),
      target  = DQN(91L, 5L)
    )
  )
  Indices <- list(
    invest = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    borrow = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    deposit = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    withdraw = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    loanrate = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    depositrate = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    approverate = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    )
  )
  return(list(Network = Network, Indices = Indices))
}

Economy_B1 <- Intermediation$new(
  "R/experiments/B1.RDS",
  firms = Firms_B1, 
  banks = Banks_B1, 
  households = Households_B1,
  dqnGen = DQN_B1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(),
  depreciation = 1
)

#debug(Economy_B1$train)
Loss <- Economy_B1$train(
  numEpisodes = 15*1024, 
  resetProb = 0.004, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)

# To reload:
# Economy_B1 <- readRDS("R/experiments/B1.RDS")
# Economy_B1$reload(lossFunc = compute_td_loss)
# Economy_B1$train(numEpisodes = 3, resetProb = 0.004, verbose = 1)
