source("R/setup.R")
source("R/utils.R")
source("R/agents/banks.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/simple_savings.R")
source("R/environments/simple_investments.R")
source_python("python/dqn.py")

library(ggplot2)
library(gganimate)
load("R/experiments/investments.RData")
load("R/experiments/savings.RData")

bankShareAnimation <- function(history, var = "deposits", col = "type",
                               main_lab = "", fill_lab = "Fill", 
                               x_lab = "bank id", y_lab = "%") {
  # input - a "EpisodeHistory" from a economy class
  # ouput - a plot to be passed to gganimate
  g <- history %>%
    map("banks") %>%
    map(
      function(banks) {
        tibble(
          bank = 1:nrow(banks), 
          val = banks[[var]] / sum(banks[[var]]),
          rate = banks[[col]]
        )
      }
    ) %>%
    reduce(rbind) %>%
    mutate(
      period = rep(
        1:length(history), 
        sapply(history, function(x) nrow(x$banks))
      )
    ) %>%
    ggplot(aes(bank, val, frame = period)) + 
    geom_col(aes(fill = as.factor(rate)), position = "identity") + 
    theme_bw() +
    labs(fill = fill_lab, main = main_lab, x = x_lab, y = y_lab)
  return(g)
}

gganimate(
  bankShareAnimation(
    Economy_S1$EpisodeHistory, 
    var = "deposits", 
    col = "depositRate", 
    main_lab = "Share of Deposits in each bank", 
    fill_lab = "interest", 
    x_lab = "Bank", 
    y_lab = "% of deposits"
  ), 
  "R/rmd/visuals/S1.gif"
)

gganimate(
  bankShareAnimation(
    Economy_S2$EpisodeHistory, 
    var = "deposits", 
    col = "type",
    main_lab = "Share of Deposits in each bank", 
    fill_lab = "bank type", 
    x_lab = "Bank", 
    y_lab = "% of deposits"
  ), 
  "R/rmd/visuals/S2.gif"
)

gganimate(
  bankShareAnimation(
    Economy_I1$EpisodeHistory, 
    var = "loans", 
    col = "loanRate",
    main_lab = "Share of Loans in each bank", 
    fill_lab = "interest", 
    x_lab = "Bank", 
    y_lab = "% of loans"
  ), 
  "R/rmd/visuals/I1.gif"
)

gganimate(
  bankShareAnimation(
    Economy_I2$EpisodeHistory, 
    var = "loans", 
    col = "type",
    main_lab = "Share of Loans in each bank", 
    fill_lab = "bank type", 
    x_lab = "Bank", 
    y_lab = "% of loans"
  ), 
  "R/rmd/visuals/I2.gif"
)

# ----
# IS <- Economy_S1$getInfoSet()[[10]]
# seq(0.1, 0.2, by = 0.01) %>%
#   lapply(function(int) {
#     IS[35] <- int
#     Economy_S1$DQN$deposit$current$act(IS, 0)
#   })
