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


linePlot <- function(
  sData,
  yvar = "value",
  xvar = "period",
  split = "series",
  title = "",
  colp = c("blue","darkblue"),
  recStart = 0,
  recEnd = 0
) {
  rects <- data.frame(xstart = recStart, xend = recEnd)
  colors <- colorRampPalette(col = colp)(length(unique(sData[[split]])))
  
  p <- ggplot() +
    geom_rect(aes(xmin = rects$xstart, xmax = rects$xend, ymin = -Inf, ymax = Inf), alpha = 0.4) + 
    geom_line(
      aes(y = sData[[yvar]], x = sData[[xvar]], colour = sData[[split]]), 
      size = 1.2, stat = "identity"
    ) +
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    labs(x = xvar, y = yvar) +
    ggtitle(title) +
    scale_colour_manual(values = colors) +
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
  
  return(p)
}

linePlot2 <- function(
  sData,
  scale_factor = 1,
  yvar1 = "value",
  yvar2 = "value2",
  xvar = "period",
  title = "",
  second_namme = yvar2,
  colp = c("blue","darkblue"),
  recStart = 0,
  recEnd = 0
) {
  rects <- data.frame(xstart = recStart, xend = recEnd)
  colors <- colorRampPalette(col = colp)(2)
  
  p <- ggplot() +
    geom_rect(aes(xmin = rects$xstart, xmax = rects$xend, ymin = -Inf, ymax = Inf), alpha = 0.4) +
    geom_line(
      aes(y = sData[[yvar1]], x = sData[[xvar]], colour = yvar1), 
      size = 1.2, stat = "identity", data = sData
    ) +
    geom_line(
      aes(y = sData[[yvar2]] * scale_factor, x = sData[[xvar]], colour = yvar2),
      size = 1.2, stat = "identity"
    ) + 
    scale_y_continuous(
      sec.axis = sec_axis(~.*(1/scale_factor), name = second_namme)
    ) + 
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    labs(x = xvar, y = yvar1) +
    ggtitle(title) +
    scale_colour_manual(values = colors) +
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
  
  return(p)
}

plotOutput <- function(History, title = "", 
                       colp = c("darkblue","blue"), recStart = 0, recEnd = 0) {
  output <- History %>% 
    map("macro") %>% 
    map("output") %>% 
    reduce(c)
  tibble(period = 1:length(output), value = output, series = "output") %>%
    linePlot(title = title, colp = colp, recStart = recStart, recEnd = recEnd)
}
#plotOutput(Economy_B1$EpisodeHistory)

plotOutputBankVar <- function(
  History, 
  bankVar = "loans",
  bankReduce = sum,
  title = "", 
  colp = c("darkblue", "blue"),
  scale_factor = 1,
  recStart = 0,
  recEnd = 0
) {
  output <- History %>% 
    map("macro") %>% 
    map("output") %>% 
    reduce(c)
  bv <- History %>%
    map("banks") %>%
    map(bankVar) %>%
    map(bankReduce) %>%
    reduce(c)
  tib <- tibble(period = 1:length(History), output = output, bankVar = bv)
  colnames(tib)[colnames(tib) == "bankVar"] <- bankVar
  tib %>% 
    linePlot2(yvar1 = "output", yvar2 = bankVar, 
              title = title, colp = colp, scale_factor = scale_factor,
              recStart = recStart, recEnd = recEnd)
}
#plotOutputBankVar(Economy_B1$EpisodeHistory, bankVar = "depositRate", bankReduce = mean, scale_factor = 3000)

plotPropensities <- function(
  net, x, inds, variable, 
  vrange, vlength = 400,
  title = "", colp = c("blue","darkblue")
) {
  findPropensity(net, x, variable, vrange, vlength, inds) %>%
    reduce(rbind) %>%
    as_tibble %>%
    as.list %>% # melt columns together
    map(as_tibble) %>%
    reduce(rbind) %>%
    mutate(
      series = as.character(rep(inds, each = vlength)), 
      predictor = rep(seq(vrange[1],vrange[2], length.out = vlength), length(inds))
    ) %>%
    linePlot(
      yvar = "value",
      xvar = "predictor",
      split = "series",
      title = title,
      colp = colp
    )
}
# plotPropensities(
#   net = Economy_B1$DQN$invest$current, 
#   x = Economy_B1$InfoSets$Firms[[20]], 
#   variable = "amount_opportunity", 
#   vrange = c(0,15), inds = c(1,2)
# )