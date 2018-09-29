source("R/setup.R")

# Test G2 ------------------------------------------------------------------
# 10 banks
Banks_G2 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 400, # 1200 before
      reserves = 400,
      capitalRatio = 0.08,
      reserveRatio = 0.10,
      dividentRatio = 0.03,
      noDividentPeriod = 18,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

# 100 firms
Firms_G2 <- lapply(
  1:100,
  function(i) {
    Firm$new(
      nBanks, 
      endowment = 15, # initial cash
      endowment_k = 1, # initial capital
      utilf = logUtility, 
      Pool = ProjectPool$new(
        duration = c(6, 25),
        amount   = c(10, 20),
        income   = c(0, 15),
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
Households_G2 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_G2),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

DQN_G2 <- function() {
  Network <- list(
    firm = list(
      current = DQN(132L, 12L), #$cuda(),
      target  = DQN(132L, 12L)
    ),
    household = list(
      current = DQN(94L, 13L),
      target  = DQN(94L, 13L)
    ),
    bank = list(
      current = DQN(91L, 27L),
      target  = DQN(91L, 27L)
    )
  )
  Indices <- list(
    firm = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    household = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    ),
    bank = list(
      current = c(1,4,7),
      target  = c(1,4,7)
    )
  )
  return(list(Network = Network, Indices = Indices))
}

Economy_G2 <- IntermediationGuarantee$new(
  "R/experiments/G2.RDS",
  firms = Firms_G2, 
  banks = Banks_G2, 
  households = Households_G2,
  dqnGen = DQN_G2, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

Economy_G2$dqnData <- readRDS("R/experiments/N2.RDS")$dqnData # hot start
Economy_G2$reload(lossFunc = compute_td_loss)

#debug(Economy_G2$train)
Loss <- Economy_G2$train(
  numEpisodes = 10*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 512, 
  batch_size = 512
)

# To reload:
# Economy_G2 <- readRDS("R/experiments/G2.RDS")
# Economy_G2$reload(lossFunc = compute_td_loss)
# Economy_G2$train(numEpisodes = 3, resetProb = 0.004, verbose = 1)
