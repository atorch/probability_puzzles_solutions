## Five foxes and seven hounds get randomly jumbled (they're in a line)
## On average, how many foxes are immediately followed by a hound?

set.seed(987987)

get_n_followed <- function(population=c(rep("fox", 5), rep("hound", 7))) {

    N <- length(population)

    # Note: this puts the foxes and hounds in random order
    jumble <- sample(population, size=N, replace=FALSE)

    ## Note: if there's a fox at the end of the line (index N),
    ## it's impossible for him to be followed by a hound
    return(sum(jumble[seq_len(N-1)] == "fox" & jumble[seq(2, N)] == "hound"))
}

n_simulations <- 10^5
simulations <- replicate(n_simulations, get_n_followed())
mean(simulations)  # Expect something close to 35/12 = 5 * (11/12) * (7/11)
t.test(simulations, mu=35/12)
