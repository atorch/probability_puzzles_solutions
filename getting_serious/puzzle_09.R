## This is the coupon collector's problem with 4 types of objects
## It can be solved with pen and paper, but let's confirm via simulation

correct_answer <- 4 * (1/4 + 1/3 + 1/2 + 1)

get_time_to_complete_collection <- function(n_objects=4) {
    seen <- rep(FALSE, n_objects)
    time <- 0
    while(sum(seen) < n_objects) {
        next_object <- sample(n_objects, size=1)
        seen[next_object] <- TRUE
        time <- time + 1
    }
    return(time)
}

n_simulations <- 100000
simulations <- replicate(n_simulations, get_time_to_complete_collection())

mean(simulations)
t.test(simulations, mu=correct_answer)
