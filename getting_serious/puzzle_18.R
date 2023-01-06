## Population of bacteria (branching process), where adults either die or live to have two children
## What is the probability of extinction starting from an initial population of 2 individuals?

(4 + c(1, -1) * sqrt(4)) / 6  # Equals 1, 1/3

## Figure shows extinction probability of 1/3 when initial population is 1
curve(1/4 + (3/4)*x^2, from=0, to=1)
abline(a=0, b=1, lty=2, col="red")
abline(v=1/3, lty=3, col="blue")

pop_goes_extinct <- function(current_pop, max_pop=10^6, pr_survive=0.75, n_children_if_survive=2) {
    n_survive <- rbinom(1, size=current_pop, prob=pr_survive)
    if(n_survive == 0) return(T)

    ## Size of the next generation
    next_pop <- n_survive * n_children_if_survive

    ## Shortcut: if the current population ever exceeds max_pop, we assume the population never goes extinct
    if(next_pop > max_pop) return(F)

    return(pop_goes_extinct(next_pop, max_pop))
}

## Simulate probability of extinction when initial population is either 1 or 2
n_sims <- 10000
mean(replicate(n_sims, pop_goes_extinct(current_pop=1)))  # 1/3
mean(replicate(n_sims, pop_goes_extinct(current_pop=2)))  # (1/3)^2 == 1/9, around 0.11
