## Fair gambling: at each step, your wealth goes up or down by $1
## with fifty-fifty probability. Your initial wealth (at step zero) is $1.
## Conditional on going broke after your eleventh bet, what's the probability that
## your wealth trajectory went straight up to $6 before going straight down to $0?

wealth_path <- function(n_bets=11, initial_wealth=1) {

    wins <- sample(c(1, -1), size=n_bets, replace=TRUE)
    wealth <- initial_wealth + cumsum(wins)

    ## Track whether we hit zero at correct time, and not before
    hit_zero <- wealth[n_bets] == 0 && all(head(wealth, n_bets - 1) > 0)

    hit_six <- max(wealth) >= 6

    return(c("hit_zero"=hit_zero, "hit_six"=hit_six))
}

n_reps <- 1000000
wealth_paths <- replicate(n_reps, wealth_path())

rowMeans(wealth_paths)
df <- data.frame(t(wealth_paths))
with(subset(df, hit_zero), mean(hit_six))

## See https://en.wikipedia.org/wiki/Catalan_number for the pen and paper solution
correct_answer <- 1/42
t.test(subset(df, hit_zero)$hit_six, mu=correct_answer)
