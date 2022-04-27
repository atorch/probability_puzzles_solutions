## Ten people, two rooms (call them A and B)
n_people <- 10

## States are {0, 1, 2, ..., 10}
## The state variable is the number of people in room B
## A state of 10 would mean everyone is in room B, for example
states <- seq(0, n_people)

## Transition matrix P
## P[i, j] is the probability of transitioning from state i to j
P <- matrix(0, length(states), length(states))
rownames(P) <- states
colnames(P) <- states
for(state_idx in seq(2, n_people)) {

    state <- states[state_idx]

    ## Person in room B selected and moves to A, so the state decreases
    P[state_idx, state_idx - 1] <- state / n_people

    ## Person in room A selected and moves to B, so the state increases
    P[state_idx, state_idx + 1] <- (n_people - state) / n_people
}

## Edge cases: when one room is full,
## one person will move to the empty room (with probability one)
P[1, 2] <- 1
P[n_people + 1, n_people] <- 1

## Each row of the transition matrix should sum to 1.0
all(rowSums(P) == 1)

## Initial distribution -- initially, the state is equally likely to be 0 or 1
pi <- c(0.5, 0.5, rep(0, length(states) - 2))

t(pi) %*% P  # Distribution after one period
t(pi) %*% P %*% P  # Distribution after two periods
t(pi) %*% P %*% P %*% P  # After three...

time_max <- 500  # What is the distribution after time_max periods?
curr_pi <- pi
for(time in seq_len(time_max)) {
    curr_pi <- curr_pi %*% P
}
curr_pi
curr_pi %*% P  # One period later -- essentially unchanged
max(abs(curr_pi - dbinom(x=states, size=max(states), prob=0.5)))  # Very small -- curr_pi is very close to binomial

## Another approach to finding the stationary distribution of this Markov chain
## See http://www.stat.yale.edu/~pollard/Courses/251.spring2013/Handouts/Chang-MarkovChains.pdf
## and http://www.mi.fu-berlin.de/wiki/pub/CompMolBio/MarkovKetten15/Lecture2.pdf
eigen_P <- eigen(t(P))
eigen_P$values[1]  # The first eigenvalue is 1.0

## "Recall that the eigenvectors are only defined up to a constant"
stationary_pi <- eigen_P$vectors[, 1] / sum(eigen_P$vectors[, 1])
max(abs(stationary_pi - dbinom(x=states, size=max(states), prob=0.5)))

## The puzzle asks for the stationary probability of 5 people in each room
stationary_pi[which(states == 5)]
choose(10, 5) / 2^10
