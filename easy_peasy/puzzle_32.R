## Six people (named A through F) will stand in line
## How many possible orderings if person A and person B refuse to stand next to each other?

library(combinat)

length(permn(letters[1:6]))  # Equals factorial(6) == 720

a_and_b_are_apart <- function(x) {
    return(abs(which(x == "a") - which(x == "b")) != 1)
}

sum(sapply(permn(letters[1:6]), a_and_b_are_apart))  # 480 == factorial(6) - 10 * factorial(4)
permn(letters[1:6])[!sapply(permn(letters[1:6]), a_and_b_are_apart)]
