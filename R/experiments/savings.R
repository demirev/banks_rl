source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/simple_savings.R")
source_python("python/dqn.py")

Banks <- lapply(
  1:3, 
  function(i) DummyBank$new(rating = sample(1:5,1))
)

Households <- lapply(
  1:20,
  function(i) VanillaHousehold$new(length(Banks))
)

DQNs <- list(
  withdraw = list(
    current = DQN(25L, 4L), #$cuda(),
    target  = DQN(25L, 4L)
  ),
  deposit = list(
    current = DQN(25L, 4L),
    target  = DQN(25L, 4L)
  )
)

Economy <- SimpleSavings$new(
  households = Households, 
  banks = Banks, 
  dqns = DQNs, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy$train(numEpisodes = 1024*4)
