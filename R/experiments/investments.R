source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/environments/constructor.R")
source("R/environments/simple_investments.R")
source_python("python/dqn.py")

Banks <- lapply(
  1:3, 
  function(i) DummyBankInv$new(rating = sample(1:5,1))
)

Firms <- lapply(
  1:20,
  function(i) {
    VanillaFirm$new(nBanks = length(Banks), endowment = 100, utilf = logUtility)
  }
)

DQNs <- list(
  invest = list(
    current = DQN(74L, 2L), #$cuda(),
    target  = DQN(74L, 2L)
  ),
  borrow = list(
    current = DQN(74L, 4L),
    target  = DQN(74L, 4L)
  )
)

Economy <- SimpleInvestments$new(
  firms = Firms, 
  banks = Banks, 
  dqns = DQNs, 
  lossFunction = compute_td_loss, 
  bufferSize = 1000L
)

Economy$train(numEpisodes = 1024*4)
