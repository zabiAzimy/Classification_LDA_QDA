# we are writing a function that computes the posterior probability

# Here we will use an R function called dnorm() to compute the denstity function
# list of arguments for the function:
# x : The value of x for which we compute the posterior probability
# pi0: Represents the prior probability
# We also mu0 and mu1 that represents the mean values of the classes
# the sigma is the variance of the normally distributed x

posterior <- function(x, pi0 = 0.5, mu0 = 4, mu1 = 5, sigma = 1) {
  (dnorm(x, mean = mu1, sd = sigma)) * (1 - pi0) / 
    (dnorm(x, mean = mu1, sd = sigma) * (1 - pi0) +
       dnorm(x, mean = mu0, sd = sigma) * pi0)
}


# now we want to plot the function using R curve() for the x values that start
# from 0 and end at 8

curve(posterior, from = 0, to = 8, lwd = 2, col = "darkgreen")


# using ggplot2 library we make better plots
# this time we play with the pi0 value and see different curves
# first load the library
library(ggplot2)

ggplot() +
  geom_function(fun = posterior, col = "black") +
  geom_function(fun = posterior,
                args = list(pi0 = 0.8), col = "red") +
  geom_function(fun = posterior,
                args = list(pi0 = 0.2), col = "blue") +
  geom_function(fun = posterior,
                args = list(mu0 = 3, mu1 = 6), col = "orange") +
  geom_function(fun = posterior,
                args = list(mu0 = 3, mu1 = 6, sigma = 2), col = "green") +
  scale_x_continuous(limits = c(0, 8))
