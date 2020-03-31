correct_solution <- 1 - 1/exp(1)

person_with_own_hat <- function(n=10^3) {
    ## Suppose people are named 1, 2, ... n and stand in that order
    people <- seq(1, n)
    hats <- sample(people, n)
    return(any(hats == people))  # Is anyone standing in front of their hat?
}
simulations <- replicate(10^5, person_with_own_hat())

t.test(simulations, mu=correct_solution)
