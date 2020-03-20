## Two players take turns rolling a six-sided die
## First to roll a 6 wins
## What's your probability of winning if you go first?

set.seed(123321)

get_win_indicator <- function(max_simulation_length=10^4) {
    dice_rolls <- sample(seq_len(6), size=max_simulation_length, replace=TRUE)

    ## Note: this is a default outcome in case we reach max_simulation_length
    if(all(dice_rolls != 6)) return(TRUE)

    index_first_six <- min(which(dice_rolls == 6))

    return(index_first_six %% 2 == 1)
}

## The exact solution solves the equation p = (1/6) + (5/6)^2*p
## p = (1/6) / (1 - (5/6)^2) = 36 / (11 * 6) = 6/11

## Let's approximate p by running lots of simulations and averaging
n_simulations <- 10^4
simulations <- replicate(n_simulations, get_win_indicator())

## This should be close to the correct p = 6/11
p_hat <- mean(simulations)

## We expect a large p-value and a confidence interval that includes 6/11
t.test(simulations, mu=6/11)
