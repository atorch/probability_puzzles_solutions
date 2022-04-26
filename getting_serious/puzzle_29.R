## Ten people, two rooms, states are {0, 1, 2, ..., 10} (number of people in other room)
states <- seq(0, 10)

## Transition matrix P
## P[i, j] is the probability of transitioning from state i to j
P <- matrix(0, length(states), length(states))
rownames(P) <- states
colnames(P) <- states
for(i in seq_along(states)[seq(2, length(states) - 1)]) {
    P[i, i-1] <- (i-1) / max(states)  # Person in other room selected, state decreases
    P[i, i+1] <- (max(states) - i + 1) / max(states)
}

## Edge cases: when one room is full,
## one person will move to the empty room (with probability one)
P[1, 2] <- 1
P[length(states), length(states) - 1] <- 1

all(rowSums(P) == 1)  # Careful with float comparisons

## Initial distribution -- initial, the state is equally likely to be 0 or 1
pi <- c(0.5, 0.5, rep(0, length(states) - 2))

t(pi) %*% P  # Distribution after one period
t(pi) %*% P %*% P  # Distribution after two periods
t(pi) %*% P %*% P %*% P  # After three...

time_max <- 100  # What is the distribution after time_max periods?
curr_pi <- pi
for(time in seq(2, time_max)) {
    curr_pi <- curr_pi %*% P
}
curr_pi
curr_pi %*% P  # One period later -- essentially unchanged
max(abs(curr_pi - dbinom(x=states, size=max(states), prob=0.5)))  # Very small -- curr_pi is very close to binomial

## Another approach to finding the stationary distribution of this Markov chain
eigen_P <- eigen(t(P))
eigen_P$values[1]  # The first eigenvalue is 1.0

## "Recall that the eigenvectors are only defined up to a constant"
stationary_pi <- eigen_P$vectors[, 1] / sum(eigen_P$vectors[, 1])
max(abs(stationary_pi - dbinom(x=states, size=max(states), prob=0.5)))

## The puzzle asks for the stationary probability of 5 people in each room
stationary_pi[which(states == 5)]
choose(10, 5) / 2^10
