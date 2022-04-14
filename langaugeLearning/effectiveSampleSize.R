# Effective sample size
df <- data.frame(
  "School" = c(rep("LIN", 7), rep("HLF", 5)),
  "Table 1" = c(0.225, NA, 0.1, 0.375, 0.4, NA, 0.067,
                0.175, 0.435, 0.1, 0.067, 0.175),
  "Table 2" = c(2.2224, 4, 0.2847, 2.0412, 2.7784, 2.1429, 1.1763, 
                0.6131, 4.0275, 0.4979, 1.5455, 2.2944),
  "Table 3" = c(0.3093, 1.3091, 0.2917, 0.1513, 0.0848, 1.3636, -0.2113, 
                0.0086, 0.3527, -0.0459, -2.2622, 0.1770),
  "Table 4" = c(-0.1143, 0.6246, -0.2732, 0.2853, -0.2179, 0.7273, 0.1292,
                0.0933, 0.0573, 0.3067, 0.1345, 0.1364),
  "Table 5" = c(-0.0101, 0.1111, -0.0088, 0.0722, 0.0293, 0.1333, -0.0339,
                0.0026, 0.0110, 0.0597, 0.0060, 0.0158),
  "Table 6" = c(-0.1367, 0.6, 0.0802, 0.2619, 0.1372, 0.75, 0.2619,
                0.2111, 0.0058, 0.4502, 0.0756, 0.1152),
  "Table 7" = c(-0.0909, 0.4545, 0.2323, 0.31, 0.147, 0.5833, 0.2148,
                0.2515, 0.1, 0.375, 0.0365, 0.1909)
)
df <- df[,-c(1, 10)]
cov(df[,-1])
cov(df, use="pairwise.complete.obs")
# If independent, then variance of mean-standardized is trace(cor(df))/dim^2 = 1/dim
# When not independent, then variance of mean-standardized is sum(cor(df))/dim^2 = 1/ESS
d <- ncol(df)
d
ESS <- d^2 / sum(cor(df, use="pairwise.complete.obs"))
ESS * 12
