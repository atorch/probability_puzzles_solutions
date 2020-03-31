## Conditional correlation
n_obs <- 10^4
df <- data.frame(x=runif(n_obs), y=runif(n_obs))
df$college <- df$x + df$y > 1
with(subset(df, college), cor(x, y))  # -1/2
with(subset(df, college), cov(x, y))  # -1/36
with(subset(df, college), plot(x, y))
