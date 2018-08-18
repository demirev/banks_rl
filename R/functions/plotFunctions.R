library(ggplot2)
library(gganimate)

linePlot <- function(
  sData,
  yvar = "value",
  xvar = "period",
  split = "series",
  title = "",
  colp = c("blue","orange"),
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
  colp = c("blue","orange"),
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

linePlotDots <- function(
  sData,
  scale_factor = 1,
  yvar1 = "value",
  yvar2 = "value2",
  xvar = "period",
  title = "",
  second_namme = yvar2,
  colp = c("blue","orange"),
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
    geom_point(
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
