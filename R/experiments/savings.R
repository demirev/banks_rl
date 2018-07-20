source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/simple_savings.R")
source_python("python/dqn.py")

# Exp S1 ------------------------------------------------------------------
# Five banks - same rating, different interest payment; 200 households

Banks_S1 <- lapply(
  c(0.15, rep(0.05, 4)),
  function(int) {
    DummyBank$new(
      depositRate = int, 
      rating = 1, 
      fixedInterest = T,
      deposits = 0,
      loans = 400,
      capital = 500,
      reserves = 100,
      provisions = 0
    )
  }
)

Households_S1 <- lapply(
  1:200,
  function(i) VanillaHousehold$new(length(Banks_S1), income = 1)
)


DQN_S1 <- list(
  withdraw = list(
    current = DQN(41L, 6L), #$cuda(),
    target  = DQN(41L, 6L)
  ),
  deposit = list(
    current = DQN(41L, 6L),
    target  = DQN(41L, 6L)
  )
)

Economy_S1 <- SimpleSavings$new(
  households = Households_S1, 
  banks = Banks_S1, 
  dqns = DQN_S1, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy_S1$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

# Exp S2 ------------------------------------------------------------------
# Five banks - four with a bad rating, one with a good rating

Banks_S2 <- lapply(
  c(1, rep(5, 4)),
  function(rat) {
    DummyBank$new(
      depositRate = 0.1, 
      rating = rat, 
      fixedInterest = T,
      deposits = 0,
      loans = 400,
      capital = 500,
      reserves = 100,
      provisions = 0
    )
  }
)

Households_S2 <- lapply(
  1:200,
  function(i) VanillaHousehold$new(length(Banks_S2), income = 1)
)


DQN_S2 <- list(
  withdraw = list(
    current = DQN(41L, 6L), #$cuda(),
    target  = DQN(41L, 6L)
  ),
  deposit = list(
    current = DQN(41L, 6L),
    target  = DQN(41L, 6L)
  )
)

Economy_S2 <- SimpleSavings$new(
  households = Households_S2, 
  banks = Banks_S2, 
  dqns = DQN_S2, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy_S2$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

save(list = c("Economy_S1", "Economy_S2"), file = "R/experiments/savings.RData")
