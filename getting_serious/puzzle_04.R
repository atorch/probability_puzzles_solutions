## When tossing a fair coin, what's the expected number of rolls until HH (two heads in a row)?

set.seed(555444)

population <- c("H", "T")

get_time_to_target <- function(target_sequence=c("H", "H"), max_rolls=200) {
    rolls <- sample(population, size=max_rolls, replace=TRUE)

    time <- length(target_sequence)

    while(time < max_rolls) {
        if(all(rolls[seq(to=time, length.out=length(target_sequence), by=1)] == target_sequence)) {
            break
        }

        time <- time + 1
    }

    # Note: this returns max_rolls if the target sequence is never found
    return(time)
}

get_time_to_target_recursive <- function(rolls=NULL, time=0, target_sequence=c("H", "H")) {
    if(is.null(rolls)) {
        # Note: these are the initial rolls
        rolls <- sample(population, size=length(target_sequence), replace=TRUE)
    }

    if(all(rolls == target_sequence)) {
        return(time + length(target_sequence))
    }

    # Note: if we haven't hit the target sequence, we generate the next roll and recurse
    next_roll <- sample(population, size=1)

    return(get_time_to_target_recursive(c(rolls[2], next_roll), time + 1))
}

n_simulations <- 10^5
simulations <- replicate(n_simulations, get_time_to_target())
t.test(simulations, mu=6)

simulations <- replicate(n_simulations, get_time_to_target_recursive())
t.test(simulations, mu=6)
