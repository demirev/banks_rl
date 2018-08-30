source("R/setup.R")

# Test R1 ------------------------------------------------------------------
# 10 banks
Banks_R1 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 1200,
      reserves = 1200,
      capitalRatio = 0.08,
      reserveRatio = 0.04,
      dividentRatio = 0.03,
      noDividentPeriod = 18,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

Banks_R2 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 1200,
      reserves = 1200,
      capitalRatio = 0.08,
      reserveRatio = 0.08,
      dividentRatio = 0.03,
      noDividentPeriod = 18,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

Banks_R3 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 1200,
      reserves = 1200,
      capitalRatio = 0.08,
      reserveRatio = 0.12,
      dividentRatio = 0.03,
      noDividentPeriod = 18,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

Banks_R4 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 1200,
      reserves = 1200,
      capitalRatio = 0.08,
      reserveRatio = 0.16,
      dividentRatio = 0.03,
      noDividentPeriod = 18,
      utilf = logUtility,
      loanIncrement = 0.0025,
      depositIncrement = 0.0025,
      approvalIncrement = 0.0025
    )
  }
)

Banks_R5 <- lapply(
  1:10, 
  function(i) { 
    Bank$new(
      approvalRate = 1,
      depositRate = 0.03,
      deposits = 0,
      loanRate = 0.04,
      loans = 0,
      capital = 1200,
      reserves = 1200,
      capitalRatio = 0.08,
      reserveRatio = 0.20,
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
Firms_R1 <- lapply(
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

Firms_R2 <- lapply(
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

Firms_R3 <- lapply(
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

Firms_R4 <- lapply(
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

Firms_R5 <- lapply(
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
Households_R1 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_R1),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

Households_R2 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_R1),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

Households_R3 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_R1),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

Households_R4 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_R4),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

Households_R5 <- lapply(
  1:500,
  function(i) { 
    Household$new(
      nBanks = length(Banks_R5),
      endowment = 100, 
      utilf = logUtility, 
      labor = 1
    )
  }
)

# DQNs
DQN_R1 <- function() {
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

# create environments
Economy_R1 <- Intermediation$new(
  "R/experiments/R1.RDS",
  firms = Firms_R1, 
  banks = Banks_R1, 
  households = Households_R1,
  dqnGen = DQN_R1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

Economy_R2 <- Intermediation$new(
  "R/experiments/R2.RDS",
  firms = Firms_R2, 
  banks = Banks_R2, 
  households = Households_R2,
  dqnGen = DQN_R1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

Economy_R3 <- Intermediation$new(
  "R/experiments/R3.RDS",
  firms = Firms_R3, 
  banks = Banks_R3, 
  households = Households_R3,
  dqnGen = DQN_R1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

Economy_R4 <- Intermediation$new(
  "R/experiments/R4.RDS",
  firms = Firms_R4, 
  banks = Banks_R4, 
  households = Households_R4,
  dqnGen = DQN_R1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

Economy_R5 <- Intermediation$new(
  "R/experiments/R5.RDS",
  firms = Firms_R5, 
  banks = Banks_R5, 
  households = Households_R5,
  dqnGen = DQN_R1, 
  lossFunction = compute_td_loss, 
  bufferSize = 2000L,
  productionFunction = CobbDouglass$new(productivity = 3),
  depreciation = 1
)

# pre-load weights
Economy_N1 <- readRDS("R/experiments/N1_1308.RDS")
Economy_R1$dqnData <- Economy_N1$dqnData
rm(Economy_N1)

Economy_R1$reload(compute_td_loss)

# train
Loss_R1 <- Economy_R1$train(
  numEpisodes = 4*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)

Economy_R2$dqnData <- Economy_R1$dqnData
rm(Economy_R1)
Economy_R2$reload(compute_td_loss)

Loss_R2 <- Economy_R2$train(
  numEpisodes = 2*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)

Economy_R3$dqnData <- Economy_R2$dqnData
rm(Economy_R2)
Economy_R3$reload(compute_td_loss)

Loss_R3 <- Economy_R3$train(
  numEpisodes = 2*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)

Economy_R4$dqnData <- Economy_R3$dqnData
rm(Economy_R3)
Economy_R4$reload(compute_td_loss)

Loss_R4 <- Economy_R4$train(
  numEpisodes = 2*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)

Economy_R5$dqnData <- Economy_R4$dqnData
rm(Economy_R4)
Economy_R5$reload(compute_td_loss)

Loss_R5 <- Economy_R5$train(
  numEpisodes = 2*1024, 
  resetProb = 0.001, 
  verbose = 1, 
  saveEvery = 256, 
  batch_size = 512
)
