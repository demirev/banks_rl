#' This script produces all figures, tables and values cited in the paper
#' 

source("R/setup.R")
library(PerformanceAnalytics) # correlations plot

# functions ---------------------------------------------------------------
# output plots
drawLinePlot <- function(sim_series, sim_recessions) {
  linePlot(
    mutate(sim_series, series = "output"), 
    yvar = "output", xvar = "period", title = "Simulated Output",
    recStart = sim_recessions$start, 
    recEnd = sim_recessions$end
  )
}

drawLineDotPlot <- function(sim_series, sim_recessions, history, scale_factor = 550) {
  tibble(
    output = sim_series$output,
    defaults = countDefaults(history[50:length(history)]),
    period = sim_series$period
  ) %>% linePlotDots(
    yvar1 = "output", yvar2 = "defaults",
    scale_factor = scale_factor,
    title = "Simulated Output",
    recStart = sim_recessions$start,
    recEnd = sim_recessions$end
  )
} 

# output vs consumption / investment
drawFourPlots <- function(sim_series, sim_recessions, sc_f = c(1,1.5,1,1)) {
  plotOC <- linePlot2(
    sim_series, 
    yvar1 = "output", 
    yvar2 = "consumption", 
    title = "Consumption",
    scale_factor = sc_f[1],
    recStart = sim_recessions$start, 
    recEnd = sim_recessions$end
  )
  
  plotOI <- linePlot2(
    sim_series, 
    yvar1 = "output", 
    yvar2 = "investment",
    title = "Investment",
    scale_factor = sc_f[2],
    recStart = sim_recessions$start, 
    recEnd = sim_recessions$end
  )
  
  # output vs loans / deposits
  plotOL <- linePlot2(
    sim_series, 
    yvar1 = "output", 
    yvar2 = "loans", 
    title = "Loans",
    scale_factor = sc_f[3],
    recStart = sim_recessions$start, 
    recEnd = sim_recessions$end
  )
  
  plotOD <- linePlot2(
    sim_series, 
    yvar1 = "output", 
    yvar2 = "deposits", 
    title = "Deposits",
    scale_factor = sc_f[4],
    recStart = sim_recessions$start, 
    recEnd = sim_recessions$end
  )
  
  grid.arrange(plotOL, plotOD, plotOI, plotOC, nrow = 4)
  
}

# output vs rates
drawOutputvsRates <- function(sim_series, sim_recessions, type = "deposit") {
  if (type == "deposit") {
    linePlot2(
      sim_series, 
      yvar1 = "output", 
      yvar2 = "depositRate", 
      scale_factor = 30000,
      recStart = sim_recessions$start, 
      recEnd = sim_recessions$end
    )
  } else if (type == "loan") {
    linePlot2(
      sim_series, 
      yvar1 = "loans", 
      yvar2 = "loanRate", 
      scale_factor = 30000,
      recStart = sim_recessions$start, 
      recEnd = sim_recessions$end
    )
  } else if (type == "approval") {
    linePlot2(
      sim_series, 
      yvar1 = "loans", 
      yvar2 = "approvalRate", 
      scale_factor = 3000,
      recStart = sim_recessions$start, 
      recEnd = sim_recessions$end
    )
  }
}

# main description --------------------------------------------------------
#' This section describes the results of the benchmark case (the
#' section titled 'emerging dynamics')

History <- readRDS("R/experiments/N2.RDS")$EpisodeHistory
History <- History[50:length(History)]
Series  <- deriveTimeSeries(History)
Series  <- Series[50:nrow(Series), ]
Series$period <- 1:nrow(Series)
Recessions <- findRecessions(Series$output, streak = 4)

drawLinePlot(Series, Recessions)
drawLineDotPlot(Series, Recessions, History, scale_factor = 650)
drawFourPlots(Series, Recessions, sc_f = c(1,3,1,1))
drawOutputvsRates(Series, Recessions, "deposit")
drawOutputvsRates(Series, Recessions, "loan")
drawOutputvsRates(Series, Recessions, "approval")

# correlations
chart.Correlation(
  select(
    Series, 
    -c(depositRate, loanRate, approvalRate, rate, numberWithdrawals, period, wage)
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


# baseline - three simulations ---------------------------------------------
#' Here I read in the three additional simulations of the benchmark case,
#' which are used to compare it to the deposit guarantee case.

# derive aggreagates
Series_N2 <- paste0("R/experiments/N2s",1:3,".RDS") %>%
  map(readRDS) %>%
  map(~ .$EpisodeHistory[50:length(.$EpisodeHistory)]) %>% # remove some burnin 
  map(deriveTimeSeries) %>%
  map(~ .[50:nrow(.), ]) %>% # some additional burnin
  map(~ mutate(., period = 1:nrow(.)))

# find 'recessions'
Recessions_N2 <- Series_N2 %>%
  map(~ findRecessions(.$output, streak = 4))


# experiment 1 ------------------------------------------------------------
#' Here I read in the results of the experiment of introducing deposit
#' guarantees, and compare them to the bencmark case

# derive aggreagates
Series_G2 <- paste0("R/experiments/G2s",1:3,".RDS") %>%
  map(readRDS) %>%
  map(~ .$EpisodeHistory[50:length(.$EpisodeHistory)]) %>%
  map(deriveTimeSeries) %>%
  map(~ .[50:nrow(.), ]) %>% # some additional burnin
  map(~ mutate(., period = 1:nrow(.)))

# find 'recessions'
Recessions_G2 <- Series_G2 %>%
  map(~ findRecessions(.$output, streak = 4))

# mean withdrawals
N2_Withdrawals <- Series_N2 %>%
  map(~ .$numberWithdrawals) %>%
  reduce(c)

G2_Withdrawals <- Series_G2 %>%
  map(~ .$numberWithdrawals) %>%
  reduce(c)

t.test(N2_Withdrawals, G2_Withdrawals)

# mean loan rate
N2_LoanRate <- Series_N2 %>%
  map(~ .$loanRate) %>%
  reduce(c)

G2_LoanRate <- Series_G2 %>%
  map(~ .$loanRate) %>%
  reduce(c)

t.test(N2_LoanRate, G2_LoanRate)

# jitter of deposit rates
N2_DepositRate <- Series_N2 %>%
  map(~ .$depositRate) %>%
  reduce(c)

G2_DepositRate <- Series_G2 %>%
  map(~ .$depositRate) %>%
  reduce(c)

t.test(N2_DepositRate, G2_DepositRate)

tibble(
  output = c(N2_DepositRate, G2_DepositRate),
  scenario = c(
    rep("baseline", length(N2_DepositRate)), 
    rep("guarantee", length(N2_DepositRate))
  )
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

# jitter of output
N2_Output <- Series_N2 %>%
  map(~ .$output) %>%
  reduce(c)

G2_Output <- Series_G2 %>%
  map(~ .$output) %>%
  reduce(c)

t.test(N2_Output, G2_Output)

tibble( 
  output = c(N2_Output, G2_Output),
  scenario = c(
    rep("baseline", length(N2_Output)), 
    rep("guarantee", length(G2_Output))
  )
) %>%
  ggplot(aes(scenario, output)) +
  #geom_point(aes(colour = scenario)) +
  geom_jitter(aes(colour = scenario)) +
  labs(x = "", y = "Output") +
  ggtitle("Difference in output") +
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
#' Here I read in all the simulations of the experiment
#' where I vary the required reserve ratio and compare the
#' means and standard deviations of the various time series

# derive aggreagates
summarizeSeries <- function(paths) {
  paths %>%
    map(readRDS) %>%
    map(~ .$EpisodeHistory[50:length(.$EpisodeHistory)]) %>%
    map(deriveTimeSeries) %>%
    map(~ .[50:nrow(.), ]) %>% # some additional burnin
    map(~ mutate(., period = 1:nrow(.))) %>%
    map(function(tb) {
      tb %>%
        summarise(
          output_m = mean(output),
          output_v = sd(output),
          deposits_m = mean(deposits),
          deposits_v = sd(deposits),
          loans_m = mean(loans),
          loans_v = sd(loans),
          consumption_m = mean(consumption),
          consumption_v = sd(consumption),
          investment_m = mean(investment),
          investment_v = sd(investment),
          deposit_interst_m = mean(depositRate),
          deposit_interest_v = sd(depositRate),
          loan_interest_m = mean(loanRate),
          loan_interest_v = sd(loanRate)
        )
    }) %>%
    reduce(rbind) %>%
    colMeans
}

summaryTable <- c(1:5) %>% # for each configuration
  map(~ paste0("R/experiments/R", .,"s",1:3,".RDS")) %>% # for each simulation
  map(summarizeSeries) %>% # summarize values
  reduce(rbind) %>% # and combine in a df
  as_tibble %>%
  mutate(rate = c(0.04, 0.08, 0.12, 0.16, 0.20))

# plot of average output
ggplot(aes(x = rate, y = output_m), data = summaryTable) +
  geom_line(color = "blue", lwt = 2) +
  geom_pointrange(
    aes(
      ymin = output_m - output_v, 
      ymax = output_m + output_v
    ), 
    color = "darkblue"
  )  +
  labs(x = "required reserves", y = "depositRate") +
  ylim(1600, 2500) +
  ggtitle("Average Output vs Required Reserves") +
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

