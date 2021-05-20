

# modelling in r : building models using simulated data sets --------------


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)
ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) + 
  geom_point()

# To compute the distances which give teh least difference between the predicted values and 
# the actual values we need to first turn our model family into an R function.
# This takes the parameters and the data as inputs, and give the values predicted by the
# model as output.

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

# We then need some way to compute an overall distance between the predicted and actual values.
# In other word we need to collapse the values into a single number
# A common way to do this is to use the "root-mean_squared deviations."
# We compute the difference between the actual and the predicted, square them, average them 
# and then take the square root .

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

# We can use purrr to compute the distance for all the models defined previously.

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

# Overlaying the 10 best models on to the data, coloring the models by -dist

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )







?map2_dbl


























