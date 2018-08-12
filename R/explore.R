source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/intermediation.R")
source_python("python/dqn.py")
source("R/plots.R")

# Economy_B1 <- readRDS("R/experiments/B1.RDS")
# Economy_B1$reload(lossFunc = compute_td_loss)
# Economy_B1$train(numEpisodes = 3, resetProb = 0.004, verbose = 1)

# remove initial periods
History <- Economy_N1$EpisodeHistory[50:length(Economy_N1$EpisodeHistory)]

# calculate recessions
output <- History %>%
  map("macro") %>%
  map("output") %>%
  reduce(c)

Recessions <- findRecessions(output, streak = 3)

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
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "deposits", 
  bankReduce = sum, 
  scale_factor = 0.1,
  recStart = Recessions$recStart, 
  recEnd = Recessions$recEnd
)

plotOutputBankVar(
  History, 
  bankVar = "reserves", 
  bankReduce = sum, 
  scale_factor = 0.04,
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
  net = Economy_N1$DQN$firm$current,
  x = Economy_N1$InfoSets$Firms[[30]],
  variable = "amount_opportunity",
  vrange = c(0,15), inds = c(3:12)
)

plotPropensities(
  net = Economy_N1$DQN$bank$current,
  x = Economy_N1$InfoSets$Banks[[1]],
  variable = "loanRate",
  vrange = c(0,0.5), inds = c(1:5)
)

plotPropensities(
  net = Economy_N1$DQN$household$current,
  x = Economy_N1$InfoSets$Households[[1]],
  variable = "depositRate_4",
  vrange = c(0,0.5), vlength = 500, inds = c(3,7)
)
