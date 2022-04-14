# Visualizations of language-learning results
rm(list=ls()); gc()
# data
results <- data.frame(
  #"Table 1" = c(),
  "T2" = c(2.2224, 4, 0.2847, 2.0412, 2.7784, 2.1429, 1.1763, 
           0.6131, 4.0275, 0.4979, 1.5455, 2.2944),
  "T3" = c(0.3093, 1.3091, 0.2917, 0.1513, 0.0848, 1.3636, -0.2113, 
           0.0086, 0.3527, -0.0459, -2.2622, 0.1770),
  "T4" = c(-0.1143, 0.6246, -0.2732, 0.2853, -0.2179, 0.7273, 0.1292,
           0.0933, 0.0573, 0.3067, 0.1345, 0.1364),
  "T5" = c(0.0, 0.9231, 0.0833, 0.1207, -0.0133, 1.0, 0.1865,
                0.2188, 0.0903, 0.3497, 0.1364, 0.0357),
  "T6" = c(-0.0101, 0.1111, -0.0088, 0.0722, 0.0293, 0.1333, -0.0339,
                0.0026, 0.0110, 0.0597, 0.0060, 0.0158),
  "T7" = c(-0.1367, 0.6, 0.0802, 0.2619, 0.1372, 0.75, 0.2619,
                0.2111, 0.0058, 0.4502, 0.0756, 0.1152),
  "T8" = c(-0.0909, 0.4545, 0.2323, 0.31, 0.147, 0.5833, 0.2148,
                0.2515, 0.1, 0.375, 0.0365, 0.1909),
  "Gruppe" = as.factor(c(rep("LIN", 7), rep("HLF", 5)))
)
library(GGally)
cbp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Coordinate plot
p <- 
  ggparcoord(
    results,
    columns = 1:7, groupColumn = 8, order = "anyClass",
    showPoints = TRUE, 
    title = "Parallel Coordinate Plot for Measurements",
    alphaLines = 1.0
  ) + 
  ylab("Standardized measurements") + 
  xlab("Table") + 
  theme_bw() +
  scale_colour_manual(values=cbp) + 
  theme(legend.position = "top")
p
setwd("C:/Users/104170belu/Documents/Language-Learning-Stats/fig")
pdf(file="parallelCoordinatePlot.pdf", width = 7, height = 5)
print(p)
dev.off()