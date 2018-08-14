source("R/setup.R")
source("R/functions/utils.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/intermediation.R")
source_python("python/dqn.py")
source("R/functions/plotFunctions.R")

Economy <- readRDS("R/experiments/N1_1308.RDS")
Economy$reload(lossFunc = compute_td_loss)

# remove initial periods
History <- Economy$EpisodeHistory[50:length(Economy$EpisodeHistory)]

# derive time series
Series <- History %>% deriveTimeSeries

# remove some more burnin
Series <- Series[50:nrow(Series), ]
Series$period <- 1:nrow(Series)

# find 'recessions'
Recessions <- findRecessions(Series$output, streak = 4)

# output plot
linePlot(
  mutate(Series, series = "output"), 
  yvar = "output", xvar = "period",
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

# output vs consumption / investment
linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "consumption", 
  scale_factor = 1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "investment", 
  scale_factor = 1.5,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

# output vs loans / deposits
linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "loans", 
  scale_factor = 1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "deposits", 
  scale_factor = 0.1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

# output vs rates
linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "depositRate", 
  scale_factor = 30000,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

linePlot2(
  Series, 
  yvar1 = "loans", 
  yvar2 = "loanRate", 
  scale_factor = 30000,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

linePlot2(
  Series, 
  yvar1 = "loans", 
  yvar2 = "approvalRate", 
  scale_factor = 3000,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

# correlations
library(PerformanceAnalytics)
chart.Correlation(
  select(
    Series, 
    -c(depositRate, loanRate, approvalRate, numberWithdrawals, period, wage)
  ), 
  histogram = TRUE, pch = 19
)

# test output for stationarity
lm(output ~ period, data = Series) %>% summary
Box.test(Series$output, lag = 20, type = "Ljung-Box")

# volatilities
Volatilities <- tibble(
  series = names(Series),
  cv = reduce(map(Series, function(x) sd(x)/mean(x)),c),
  ac = reduce(map(Series, function(x) acf(x)$acf[2]),c),
  cr = reduce(map(Series, function(x) cor(x, Series$output)),c)
)

# lags and leads
Comovements <- Series %>%
  map(function(x) ccf(Series$output, x, lag.max = 6)$acf) %>%
  reduce(rbind) %>%
  as_tibble
Comovements <- cbind(names(Series), Comovements) %>% as_tibble
colnames(Comovements) <- c("series", -6:6)

# output vs bank defaults
# tibble(
#   value = Series$output, 
#   value2 = countDefaults(History[50:length(History)]),
#   period = Series$period
# ) %>% linePlotDots(
#   scale_factor = 550,
#   recStart = Recessions$start,
#   recEnd = Recessions$end
# )
# 
# # propensities
# plotPropensities(
#   net = Economy$DQN$firm$current,
#   x = Economy$InfoSets$Firms[[40]],
#   variable = "loanRate_3",
#   vrange = c(0,0.45), inds = c(3,5)
# )
# 
# plotPropensities(
#   net = Economy$DQN$bank$current,
#   x = Economy$InfoSets$Banks[[1]],
#   variable = "depositRate",
#   vrange = c(0,0.5), inds = c(1:5)
# )
# 
# plotPropensities(
#   net = Economy$DQN$household$current,
#   x = Economy$InfoSets$Households[[1]],
#   variable = "depositRate_2",
#   vrange = c(0,0.15), vlength = 500, inds = c(1,5)
# )
