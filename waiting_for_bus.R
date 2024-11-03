simulation <- function(n_obs=10000, lambda=2) {
    ## Arrival time uniform [0, 1]
    ## The previous bus arrived at time 0; the next bus arrives at time 1
    ## You don't know when you arrived, but you observe y people in line
    ## Arrivals in line are distributed (Y | X=x) ~ Poisson(lambda * x)

    df <- data.frame(x=runif(n_obs))
    df$time_until_bus <- 1 - df$x

    ## Number of people Y=y observed in line
    df$y <- rpois(n=n_obs, lambda=lambda * df$x)
    return(df)
}

df <- simulation()

## Intuitively, the more people y are observed in line, the larger is E[X | Y=y]
plot(df$x, df$y)

## I have an analytical expression for Pr[ X <= x and Y = y ]
## Check for correctness via simulation

analytical_pr_Xleqx_and_Yeqy <- function(x, y, lambda=2) {
    partial_sum_k <- seq(from=0, to=y)
    partial_sum_terms <- ((lambda * x) ^ partial_sum_k) / factorial(partial_sum_k)
    partial_sum <- sum(partial_sum_terms)
    return((lambda ^ y) * (x^y) * ((lambda * x)^(-y)) * (factorial(y) - factorial(y) * exp(-lambda * x) * partial_sum) /
           (factorial(y) * lambda))
}

analytical_pr_Xleqx_and_Yeqy_v2 <- function(x, y, lambda=2) {
    partial_sum_k <- seq(from=0, to=y)
    partial_sum_terms <- ((lambda * x) ^ partial_sum_k) / factorial(partial_sum_k)
    partial_sum <- sum(partial_sum_terms)
    return((1 - exp(-lambda * x) * partial_sum) / lambda)
}

analytical_pr_Yeqy <- function(y, lambda=2) {
    partial_sum_k <- seq(from=0, to=y)
    partial_sum_terms <- ((lambda) ^ partial_sum_k) / factorial(partial_sum_k)
    partial_sum <- sum(partial_sum_terms)
    return((lambda ^ y) * ((lambda)^(-y)) * (factorial(y) - factorial(y) * exp(-lambda) * partial_sum) /
           (factorial(y) * lambda))
}

## What is the probability that X <= 0.7 and y=y?
analytical_pr_Xleqx_and_Yeqy(x=0.7, y=2)
analytical_pr_Xleqx_and_Yeqy_v2(x=0.7, y=2)
mean(df$x <= 0.7 & df$y == 2)

analytical_pr_Xleqx_and_Yeqy(x=1.0, y=2)
analytical_pr_Xleqx_and_Yeqy_v2(x=1.0, y=2)
mean(df$x <= 1.0 & df$y == 2)

analytical_pr_Xleqx_and_Yeqy(x=0.7, y=3)
mean(df$x <= 0.7 & df$y == 3)

## These are all close! Looks very promising
analytical_pr_Xleqx_and_Yeqy(x=1.0, y=3)
analytical_pr_Yeqy(y=3)
mean(df$x <= 1.0 & df$y == 3)
mean(df$y == 3)

analytical_pr_Xleqx_and_Yeqy(x=1.0, y=4)
analytical_pr_Yeqy(y=4)
mean(df$x <= 1.0 & df$y == 4)
mean(df$y == 4)


## Next steps:
## We have analytical Pr[ X <= x and Y = y]
## Next step is to get Pr[ X <= x | Y = y]
## Then we can compute the density by dif wrt x
## Then we can compute E[ X | Y = y] by integrating that density

Pr[ X <= x | Y = y] = Pr[ X <= x and Y = y] / Pr[ Y = y]

Pr[ X <= x | Y = y] = Pr[ X <= x and Y = y] / Pr[ X <= 1.0 and Y = y]
