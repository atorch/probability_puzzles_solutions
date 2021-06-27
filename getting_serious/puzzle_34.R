library(ggplot2)
library(sp)

random_points_on_circle <- function(n_points) {
    theta <- runif(n=n_points, min=0, max=2*pi)
    return(cbind(cos(theta), sin(theta)))
}

triangle_points <- random_points_on_circle(3)

theta_seq <- seq(0, 2*pi, length.out=1000)
circle <- cbind(cos(theta_seq), sin(theta_seq))

plot(circle[, 1], circle[, 2], type="l")
points(0, 0, pch=2)
points(c(triangle_points[, 1], triangle_points[1, 1]), c(triangle_points[, 2], triangle_points[1, 2]), type="l", col="red")

df <- data.frame(x=circle[, 1], y=circle[, 2])
df_triangle <- data.frame(x=c(triangle_points[, 1], triangle_points[1:2, 1]), y=c(triangle_points[, 2], triangle_points[1:2, 2]))

p <- (ggplot(df, aes(x=x, y=y)) +
      geom_path(size=1.5) +
      geom_path(size=1.25, color="red", data=df_triangle) +
      geom_point(aes(x=0.0, y=0.0), colour="blue", shape=3, size=3.5) +
      theme_void())
p
ggsave("circle_and_triangle.png", plot=p, width=4, height=4)

## Does this particular triangle contain the origin?
point.in.polygon(0, 0, triangle_points[, 1], triangle_points[, 2])

random_triangle_contains_origin <- function() {
    triangle_points <- random_points_on_circle(3)
    return(point.in.polygon(0, 0, triangle_points[, 1], triangle_points[, 2]))
}

reps_random_triangle_contains_origin <- replicate(10000, random_triangle_contains_origin())

## The correct solution can be found analytically (with pen and paper)
## Let's check whether our simulation results are close
correct_solution <- 1/4
t.test(reps_random_triangle_contains_origin, mu=correct_solution)
