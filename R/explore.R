source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/intermediation.R")
source_python("python/dqn.py")
source("R/plots.R")

Economy_B1 <- readRDS("R/experiments/B1.RDS")
Economy_B1$reload(lossFunc = compute_td_loss)
Economy_B1$train(numEpisodes = 3, resetProb = 0.004, verbose = 1)

# remove initial periods
History <- Economy_B1$EpisodeHistory[25:length(Economy_B1$EpisodeHistory)]

# calculate recessions
output <- History %>%
  map("macro") %>%
  map("output") %>%
  reduce(c)

Recessions <- findRecessions(output, streak = 2)

# output plot
plotOutput(
  History, 
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

# output vs rates
plotOutputBankVar(
  History, 
  bankVar = "depositRate", 
  bankReduce = mean, 
  scale_factor = 4000,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "loanRate", 
  bankReduce = mean, 
  scale_factor = 4000,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "approvalRate", 
  bankReduce = mean, 
  scale_factor = 400,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

# output vs loans / deposits
plotOutputBankVar(
  History, 
  bankVar = "loans", 
  bankReduce = sum, 
  scale_factor = 4,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "deposits", 
  bankReduce = sum, 
  scale_factor = 1,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "capital", 
  bankReduce = sum, 
  scale_factor = 0.2,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

# output vs bank defaults
tibble(
  value = output, 
  value2 = countDefaults(History),
  period = 1:length(output)
) %>% linePlotDots(
  scale_factor = 150,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

# propensities
plotPropensities(
  net = Economy_B1$DQN$invest$current,
  x = Economy_B1$InfoSets$Firms[[30]],
  variable = "amount_opportunity",
  vrange = c(0,15), inds = c(1,2)
)

plotPropensities(
  net = Economy_B1$DQN$loanrate$current,
  x = Economy_B1$InfoSets$Banks[[1]],
  variable = "loanRate",
  vrange = c(0,0.5), inds = c(1:5)
)

plotPropensities(
  net = Economy_B1$DQN$depositrate$current,
  x = Economy_B1$InfoSets$Banks[[1]],
  variable = "depositRate",
  vrange = c(0,0.5), inds = 1:5
)

plotPropensities(
  net = Economy_B1$DQN$approverate$current,
  x = Economy_B1$InfoSets$Banks[[1]],
  variable = "approvalRate",
  vrange = c(0.5,1.5), vlength = 500, inds = 1:5
)

plotPropensities(
  net = Economy_B1$DQN$deposit$current,
  x = Economy_B1$InfoSets$Households[[1]],
  variable = "depositRate_1",
  vrange = c(0,0.5), vlength = 500, inds = 7
)
