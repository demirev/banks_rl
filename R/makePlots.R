source("R/setup.R")

# main description --------------------------------------------------------

Economy <- readRDS("R/experiments/N1_1308.RDS")
#Economy$reload(lossFunc = compute_td_loss)

# remove initial periods
History <- Economy$EpisodeHistory[50:length(Economy$EpisodeHistory)]
rm(Economy)

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

tibble(
  output = Series$output,
  defaults = countDefaults(History[50:length(History)]),
  period = Series$period
) %>% linePlotDots(
  yvar1 = "output", yvar2 = "defaults",
  scale_factor = 550,
  title = "Simulated Output",
  recStart = Recessions$start,
  recEnd = Recessions$end
)

# output vs consumption / investment
plotOC <- linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "consumption", 
  title = "Consumption",
  scale_factor = 1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

plotOI <- linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "investment",
  title = "Investment",
  scale_factor = 1.5,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

# output vs loans / deposits
plotOL <- linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "loans", 
  title = "Loans",
  scale_factor = 1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

plotOD <- linePlot2(
  Series, 
  yvar1 = "output", 
  yvar2 = "deposits", 
  title = "Deposits",
  scale_factor = 0.1,
  recStart = Recessions$start, 
  recEnd = Recessions$end
)

grid.arrange(plotOL, plotOD, plotOI, plotOC, nrow = 4)

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


# experiment 1 ------------------------------------------------------------
Economy_G1 <- readRDS("R/experiments/G1_1608.RDS")
#Economy_G1$reload(lossFunc = compute_td_loss)

# remove initial periods
History_G1 <- Economy_G1$EpisodeHistory[25:length(Economy_G1$EpisodeHistory)]
rm(Economy_G1)

# derive time series
Series_G1 <- History_G1 %>% deriveTimeSeries

# remove some more burnin
Series_G1 <- Series_G1[50:nrow(Series_G1), ]
Series_G1$period <- 1:nrow(Series_G1)

# find 'recessions'
Recessions_G1 <- findRecessions(Series_G1$output, streak = 4)

# output plot
linePlot(
  mutate(Series_G1, series = "output"), 
  yvar = "output", xvar = "period",
  recStart = Recessions_G1$start, 
  recEnd = Recessions_G1$end
)

tibble(
  output = Series_G1$output,
  defaults = countDefaults(History_G1[50:length(History_G1)]),
  period = Series_G1$period
) %>% linePlotDots(
  yvar1 = "output", yvar2 = "defaults",
  scale_factor = 550,
  title = "Simulated Output",
  recStart = Recessions_G1$start,
  recEnd = Recessions_G1$end
)


# correlations
library(PerformanceAnalytics)
chart.Correlation(
  select(
    Series_G1, 
    -c(depositRate, loanRate, approvalRate, numberWithdrawals, period, wage)
  ), 
  histogram = TRUE, pch = 19
)

# test output for stationarity
lm(output ~ period, data = Series_G1) %>% summary
Box.test(Series_G1$output, lag = 20, type = "Ljung-Box")

# volatilities
Volatilities_G1 <- tibble(
  series = names(Series_G1),
  cv = reduce(map(Series_G1, function(x) sd(x)/mean(x)),c),
  ac = reduce(map(Series_G1, function(x) acf(x)$acf[2]),c),
  cr = reduce(map(Series_G1, function(x) cor(x, Series_G1$output)),c)
)

# output vs rates
linePlot2(
  Series_G1, 
  yvar1 = "output", 
  yvar2 = "depositRate", 
  scale_factor = 1,
  recStart = Recessions_G1$start, 
  recEnd = Recessions_G1$end
)

linePlot2(
  Series_G1, 
  yvar1 = "loans", 
  yvar2 = "loanRate", 
  scale_factor = 30000,
  recStart = Recessions_G1$start, 
  recEnd = Recessions_G1$end
)

linePlot2(
  Series_G1, 
  yvar1 = "loans", 
  yvar2 = "approvalRate", 
  scale_factor = 3000,
  recStart = Recessions_G1$start, 
  recEnd = Recessions_G1$end
)

tibble(
  output = c(Series$output, Series_G1$output),
  scenario = c(rep("baseline", nrow(Series)), rep("guarantee", nrow(Series_G1)))
) %>%
  ggplot(aes(scenario, output)) +
  #geom_point(aes(colour = scenario)) +
  geom_jitter(aes(colour = scenario))


tibble(
  output = c(Series$numberWithdrawals, Series_G1$numberWithdrawals),
  scenario = c(rep("baseline", nrow(Series)), rep("guarantee", nrow(Series_G1)))
) %>%
  ggplot(aes(scenario, output)) +
  #geom_point(aes(colour = scenario)) +
  geom_jitter(aes(colour = scenario)) +
  labs(x = "", y = "Deposit withdrawals") +
  ggtitle("Difference in deposit withdrawals") +
  scale_colour_manual(values = c("blue","orange")) +
  theme(
    #axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    text = element_text(family = "serif"),
    axis.text.x = element_text(colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    legend.key = element_rect(fill = "white", colour = "white")
  )


tibble(
  output = c(Series$depositRate, Series_G1$depositRate),
  scenario = c(rep("baseline", nrow(Series)), rep("guarantee", nrow(Series_G1)))
) %>%
  ggplot(aes(scenario, output)) +
  #geom_point(aes(colour = scenario)) +
  geom_jitter(aes(colour = scenario)) +
  labs(x = "", y = "interest on deposits") +
  ggtitle("Difference in average interest rates on deposits") +
  scale_colour_manual(values = c("blue","orange")) +
  theme(
    #axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    text = element_text(family = "serif"),
    axis.text.x = element_text(colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    legend.key = element_rect(fill = "white", colour = "white")
  )

# experiment 2 ------------------------------------------------------------
History_R <- list()
R1 <- readRDS("R/experiments/R1.RDS")
R2 <- readRDS("R/experiments/R2.RDS")
R3 <- readRDS("R/experiments/R3.RDS")
R4 <- readRDS("R/experiments/R4.RDS")
R5 <- readRDS("R/experiments/R5.RDS")

History_R[[1]] <- R1$EpisodeHistory[900:length(TT$EpisodeHistory)]
History_R[[2]] <- R2$FullHistory[[4]][600:length(TT$FullHistory[[4]])]
History_R[[3]] <- R3$EpisodeHistory[300:length(TT$EpisodeHistory)]
History_R[[4]] <- R4$FullHistory[[4]][1000:length(TT$FullHistory[[4]])]
History_R[[5]] <- R5$EpisodeHistory[30:length(TT$EpisodeHistory)]

Series_R <- History_R %>%
  map(deriveTimeSeries) %>%
  map(function(x) x[20:nrow(x), ])

Series_R[[1]]$reserveRatio <- 0.04
Series_R[[2]]$reserveRatio <- 0.08
Series_R[[3]]$reserveRatio <- 0.12
Series_R[[4]]$reserveRatio <- 0.16
Series_R[[5]]$reserveRatio <- 0.20

Data_R <- Series_R %>% reduce(rbind)

lm(deposits ~ . - wage, data = Data_R) %>%
  summary
lm(output ~ reserveRatio, data = Data_R) %>% summary  
lm(consumption ~ reserveRatio, data = Data_R) %>% summary  

lm(output ~ lag(output) + lag(capital) + lag(depositRate) + lag(loanRate) + lag(rate) +
     lag(deposits) + lag(loans) + lag(numberWithdrawals) +
     lag(numberDefaults) + lag(consumption) + lag(investment) + 
     I(reserveRatio > 0.04), data = Data_R) %>%
  summary

library(PerformanceAnalytics)
chart.Correlation(
  select(
    Data_R, 
    -wage
  ), 
  histogram = TRUE, pch = 19
)

Data_R %>%
  group_by(reserveRatio) %>%
  summarise(sd = sd(deposits), mean = mean(deposits)) %>%
  ggplot(aes(x = reserveRatio, y = mean)) +
  geom_line(color = "blue", lwt = 2) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), color = "darkblue")  +
  labs(x = "required reserves", y = "deposits") +
  ggtitle("Average Deposits vs Required Reserves") +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    plot.title = element_text(size = 14, family = "serif", face = "bold"),
    text = element_text(family = "serif"),
    axis.text.x = element_text(colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    legend.key = element_rect(fill = "white", colour = "white")
  )


Data_R %>%
  group_by(reserveRatio) %>%
  summarise(sd = sd(depositRate), mean = mean(depositRate)) %>%
  ggplot(aes(x = reserveRatio, y = mean)) +
  geom_line(color = "blue", lwt = 2) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), color = "darkblue")  +
  labs(x = "required reserves", y = "depositRate") +
  ggtitle("Average depositRate vs Required Reserves") +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    plot.title = element_text(size = 14, family = "serif", face = "bold"),
    text = element_text(family = "serif"),
    axis.text.x = element_text(colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    legend.key = element_rect(fill = "white", colour = "white")
  )

Data_R %>% group_by(reserveRatio) %>% summarise_all(c("mean" = mean, "sd" = sd))
