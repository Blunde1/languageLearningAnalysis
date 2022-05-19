# Summary radial plot
library(tidyr)
library(dplyr)
library(ggplot2)
library(lazyeval) # interp function
library(purrr)


boot.t.test <- function(x, y, B=100000, alpha=0.05){
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  mxy <- mean(c(x,y))
  tStatistic <- t.test(x,y)$statistic
  # Data satisfying H0
  xh0 <- x-mx+mxy
  yh0 <- y-my+mxy
  tStatisticBoot <- numeric(B)
  meanH0Boot <- numeric(B)
  for(b in 1:B){
    xb <- sample(xh0, nx, replace = TRUE)
    yb <- sample(yh0, ny, replace=TRUE)
    tStatisticBoot[b] <- t.test(xb, yb)$statistic
    meanH0Boot[b] <- mean(c(xb, yb))
  }
  pvalue <- (1 + sum(abs(tStatisticBoot)>abs(tStatistic))) / (B+1)
  CI <- quantile(meanH0Boot, c(alpha/2, 1-alpha/2))
  res <- list(
    pvalue = pvalue,
    CI = CI
  )
  return(res)
}


df <- data.frame(
  "School" = c(rep("LIN", 7), rep("HLF", 5)),
  "Table 1" = c(0.225, NA, 0.1, 0.375, 0.4, NA, 0.067,
                0.175, 0.433, 0.1, 0.067, 0.175),
  "Table 2" = c(2.2224, 4, 0.2847, 2.0412, 2.7784, 2.1429, 1.1763, 
                0.6131, 4.0275, 0.4979, 1.5455, 2.2944),
  "Table 3" = c(0.3093, 1.3091, 0.2917, 0.1513, 0.0848, 1.3636, -0.2113, 
                0.0086, 0.3527, -0.0459, -2.2622, 0.1770),
  "Table 4" = c(-0.1143, 0.6246, -0.2732, 0.2853, -0.2179, 0.7273, 0.1292,
                0.0933, 0.0573, 0.3067, 0.1345, 0.1364),
  "Table 5" = c(0.06, 0.9231, 0.0833, 0.1207, -0.0133, 1.0, 0.1865,
                0.2188, 0.0903, 0.3497, -0.1364, 0.0357), # -0.1364 var ny
  "Table 6" = c(-0.0101, 0.1111, -0.0088, 0.0722, 0.0293, 0.1333, -0.0339,
                0.0026, 0.0110, 0.0597, 0.0060, 0.0158),
  "Table 7" = c(-0.1367, 0.6, 0.0802, 0.2619, 0.1372, 0.75, 0.2619,
                0.2111, 0.0058, 0.4502, 0.0756, 0.1152),
  "Table 8" = c(-0.0909, 0.4545, 0.2323, 0.31, 0.147, 0.5833, 0.2148,
                0.2515, 0.1, 0.375, -0.0365, 0.1909)
)
names(df) = c("school", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8")
df <- df %>% mutate(school=factor(school, levels=unique(school)))
test_cols <- names(df)[-1]


# Summary 
df %>% summarise_at(test_cols, mean, na.rm=TRUE)
df %>% summarise_at(test_cols, sd, na.rm=TRUE)
df[,2:9] <- sapply(1:8, function(i) (df[,i+1] - mean(df[,i+1], na.rm=T))/sd(df[,i+1], na.rm=T))
# Long
df_long <- df %>%
  gather(metric, measurement, `T1`:`T8`, factor_key=TRUE)
#data_long <- gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)
# Create summary 
ADJUST = 5
sd_boot <- function(x)
{
  B <- 1000
}
lower <- function(x, adjust=ADJUST)
{
  mean(x, na.rm=T) - 1.96*sd(x, na.rm=T) / sqrt(length(x)) + adjust
}
upper <- function(x, adjust=ADJUST)
{
  mean(x, na.rm=T) + 1.96*sd(x, na.rm=T) / sqrt(length(x)) + adjust
}
mean_plus <- function(x, adjust=ADJUST)
{
  mean(x, na.rm=T) + adjust
}
df_summary <- df_long %>%
  group_by(metric) %>%
  summarise_at("measurement", list(lower=lower, upper=upper))
df_summary
df_poly <- df_summary %>%
  gather(type_edge, measure_edge, lower:upper)
df_means <- df %>%
  group_by(school) %>%
  summarise_at(test_cols, mean_plus) %>%
  gather(measure, measurement, `T1`:`T8`, factor_key=TRUE)
df_means[df_means$school=="LIN",]
df_means[df_means$school=="HLF",]
# function requires 
rotate_data <- function(data, col, by_col) {
  lev <- levels(data[,by_col][[1]])
  num <- length(lev)
  dir <- rep(seq(((num - 1) * 360 / num), 0, length.out = num))
  data$dir_ <- map_dbl(1:nrow(data), function(x) {dir[match(data[x,by_col][[1]], lev)]})
  #col_num <- match("mpg", colnames(cars))
  #filter_criteria <- interp(~ which_column == col_num, which_column = as.name(col))
  expr <- lazyeval::interp(~x, x = as.name(col))
  data <- mutate_(data, .dots = setNames(list(expr), "plotX"))
  data <- mutate_(data, .dots = setNames(list(expr), "plotY"))
  data <- data %>%
    mutate(plotX = round(cos(dir_ * pi / 180) * plotX, 2),
           plotY = round(sin(dir_ * pi / 180) * plotY, 2))
  data
} 
# data points
confidence_band <- rotate_data(df_poly, "measure_edge", "metric")
confidence_band$id=1L
#confidence_band <- confidence_band[c(1:7,1,8:14,8),]
sjekk2 <- rotate_data(df_means[df_means$school=="LIN",], "measurement", "measure")
sjekk3 <- rotate_data(df_means[df_means$school=="HLF",], "measurement", "measure")
lim = max(df_poly$measure_edge*1.108)
line_length <- lim - 1
rl <- data_frame(dir = unique(sjekk2$dir_), l = rep(line_length, length(unique(sjekk2$dir_)))) %>% 
  mutate(plotX = cos(dir * pi / 180) * (l),
         plotY = sin(dir * pi / 180) * (l))
rl$xend <- 0
rl$yend <- 0
lb <- rl
lb$label <- levels(sjekk2$measure)
circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=TRUE){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  df <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
    df <- rbind(df, center)
  }
  return(df)
}
circlegrid <- data_frame(dia = seq(lim / 4, 2 * lim, lim / 4))
circlegrid <- circlegrid %>% 
  mutate(data = map(dia, function(x) {
    df     <- circleFun(diameter = x, filled = FALSE)
    df$lev <- x
    df
  }))
plotcircles <- bind_rows(circlegrid$data)
plotcircles$lev <- as.factor(plotcircles$lev)
cl <- data_frame(x = as.numeric(levels(plotcircles$lev)), label = as.character(round(x - ADJUST,1)))
cl <- cl[cl$x <= lim,]
middle <- circleFun(diameter = 1, start=0, end=2, filled = FALSE)
# plot each layer with its own data and aesthetics
cbp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colsLegend <- c("Helsfyr"=cbp[2], "LIN"=cbp[3])
fillLegend <- c("95% konfidensintervall"="grey70")
p <- 
  ggplot() + 
  geom_segment(data = rl, aes(x = plotX, xend = xend, y = plotY, yend = yend), colour = cbp[1]) +
  geom_path   (data = plotcircles, aes(x = x, y = y, group = lev), colour = "grey50") + 
  geom_text   (data = cl, aes(x = -x, y = 1, label = label), colour = "grey40") +
  geom_polygon(data = middle, aes(x, y), fill = "grey50", colour = "black") + 
  geom_polygon(data = confidence_band, aes(y = plotY, x = plotX, group=id, subgroup=type_edge, fill="95% konfidensintervall"), 
               size = 1, alpha = 0.6) +
  geom_path(data=sjekk2[c(1:nrow(sjekk2),1),], aes(y = plotY, x = plotX, colour = "LIN"), size = 1) +
  geom_point(data=sjekk2, aes(y = plotY, x = plotX), stat='identity', colour = cbp[3], size = 1) +
  geom_path(data=sjekk3[c(1:nrow(sjekk3),1),], aes(y = plotY, x = plotX, colour = "Helsfyr"), size = 1, linetype = "dashed") +
  geom_point(data=sjekk3, aes(y = plotY, x = plotX), stat='identity', colour = cbp[2], size = 1) +
  geom_text   (data = lb, aes(x = 1.2*plotX, y = 1.2*plotY, label = label), colour = "black") +
  ylim(-lim*1.2, lim*1.2) + xlim(-lim*1.2, lim*1.2) +
  scale_fill_manual(name=NULL, values=fillLegend) + 
  scale_color_manual(name="Gruppe", values=colsLegend) + 
  theme(
    axis.text  = element_blank(), 
    axis.title = element_blank(), 
    line       = element_blank(),
    rect       = element_blank()
  ) + 
  coord_equal()
  #ggtitle("Radial-summary-plot of Measurements", 
  #        subtitle="Further explanations / Measurements are scaled ++")
p
setwd("C:/Users/104170belu/Documents/Language-Learning-Stats/fig")
pdf(file="radialTablePlot.pdf", width = 6, height = 3.5)
print(p)
dev.off()
png(file="radialTablePlot.png", width=480, height=280)
print(p)
dev.off()
