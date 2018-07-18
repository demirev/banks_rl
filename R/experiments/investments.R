source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/environments/constructor.R")
source("R/environments/simple_investments.R")
source_python("python/dqn.py")

# Exp I1 ------------------------------------------------------------------
# 5 banks, same rating, one with lower interest rates; 50 firms
Banks_I1 <- lapply(
  c(0.07, rep(0.14, 4)), 
  function(int) DummyBankInv$new(rating = 1, loanRate = int, fixedInterest = T)
)

Firms_I1 <- lapply(
  1:50,
  function(i) {
    VanillaFirm$new(nBanks = length(Banks_I1), endowment = 100, utilf = logUtility)
  }
)

DQN_I1 <- list(
  invest = list(
    current = DQN(90L, 2L), #$cuda(),
    target  = DQN(90L, 2L)
  ),
  borrow = list(
    current = DQN(90L, 6L),
    target  = DQN(90L, 6L)
  )
)

Economy_I1 <- SimpleInvestments$new(
  firms = Firms_I1, 
  banks = Banks_I1, 
  dqns = DQN_I1, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy_I1$train(numEpisodes = 1024*2, resetProb = 0.008, verbose = 1)

# Exp I2 ------------------------------------------------------------------
# 5 banks, one bad rating, same interest; 50 firms
Banks_I2 <- lapply(
  c(5, rep(1, 4)), 
  function(rat) DummyBankInv$new(rating = rat, loanRate = 0.1, fixedInterest = T)
)

Firms_I2 <- lapply(
  1:50,
  function(i) {
    VanillaFirm$new(nBanks = length(Banks_I2), endowment = 100, utilf = logUtility)
  }
)

DQN_I2 <- list(
  invest = list(
    current = DQN(90L, 2L), #$cuda(),
    target  = DQN(90L, 2L)
  ),
  borrow = list(
    current = DQN(90L, 6L),
    target  = DQN(90L, 6L)
  )
)

Economy_I2 <- SimpleInvestments$new(
  firms = Firms_I2, 
  banks = Banks_I2, 
  dqns = DQN_I2, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy_I2$train(numEpisodes = 1024*2, resetProb = 0.008, verbose = 1)

save(list = c("Economy_I1", "Economy_I2"), file = "R/experiments/investments.RData")
