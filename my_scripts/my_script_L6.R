# You need a theoretical argument behind how the world works, the model simply helps you understand it
# Q: Is finding out a probability distribution that best fits the data a model...? 
# --> A: Yes, but then you are not really predicting, or you get the same prediction for the same value
# Q: The prior of the parameters assumes a probability or a probability distribution? Is the prior a number or a distribution? 
# --> A: it is a probability distribution over the parameters before having seen the data

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


# Prior predictive check basically guards against starting with useless values and checks whether the prior you set actually makes sense in terms of what it is predicting 

# 1. Define the prior 
# 2. Sample from the prior (used to not have to integrate over the prior, but can still get a representative result)
# 3. With the sample of parameters, generate data and check whether the results being obtained make sense

# DO EXERCISE FROM THE SLIDES AND NYCFLIGHTS
# Code the variable arr_delay into 0,1 (if it is delayed by 10min or more), then define the prior for whether it is delayed or not and then use code for the tossing example, add number of delays and this is the variable being predicted with the binomial distribution from previous class
library(nycflights13)
library(tidyverse)
#View(flights)

# Step 1. Get data
data <- flights %>% filter(!is.na(arr_delay))
data <- if_else(data$arr_delay >= 10, 1,0)
sum_delays <- sum(data)

# Step 2. set prior (with beta distribution or something between 0 and 1)

candidates <- seq(0,1,.1)


# Step 3. inference 

## likelihood 
lh <- dbinom(sum_delays, nrow(flights), candidates)

## normalized posterior
lh*prior/sum(lh*prior)

