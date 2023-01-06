## Spider following a random walk along the edges of a cube
## How long until it eats an ant that is initially at the opposite corner (and never moves)?

## State variable is the distance between the spider and the ant
spider_transitions <- rbind(c(0, 1, 0, 0),
                            c(1/3, 0, 2/3, 0),
                            c(0, 2/3, 0, 1/3),
                            c(0, 0, 0, 1))
time_to_reach <- function(P=spider_transitions) {
    stopifnot(all(rowSums(P) == 1))
    state <- 1
    time <- 0
    while(state != 4) {
        time <- time + 1
        state <- sample(1:4, size=1, prob=P[state, ])
    }
    return(time)
}
mean(replicate(10^5, time_to_reach()))  # Around 10
P <- spider_transitions[1:3, 1:3]
solve(diag(3) - P, rep(1, 3))  # 10, 9, 7
