# You need a theoretical argument behind how the world works, the model simply helps you understand it
# Q: Is finding out a probability distribution that best fits the data a model..? 
# Q: The prior of the parameters assumes a probability or a probability distribution? Is the prior a number or a distribution? --> A: it is a probability distribution over the parameters before having seen the data

# Exercise 1------------
# Uniform, normal, beta, exponential, gamma --> which of these ones best captures my current knowledge of the phenomenon
# 1: mean and sd of income of people in LA
x_min <- 50000
x_max <- 120000
range <- seq(x_min, x_max, length.out = 100) # range
d <- dnorm(range, mean = 80000, sd = 5000) # densities
NORM <- data.frame(range, d)

ggplot(NORM, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff") +
  labs(x = expression(mu_j),
       y = "Density")
