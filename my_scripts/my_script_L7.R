# load packages
library(tidyverse)
library(rethinking)


#distribution parameters
N <- 1e3
a <- 2
b <-4

# Repeat and then plot
hist(replicate(N, sum(rbeta(N, a, b))))
hist(replicate(N, sum(rgamma(N, a, b))))
hist(replicate(N, sum(runif(N, a, b))))


# Priors for the motel PTS_i ~ N(mu, sigma)
# For the mean of points scored in a game a normal distribution with mean 30
# and sd of 5, for the sd of points per game gamma bc it shouldnt take 
# negative values
hist(rnorm(N, mean = 30, sd = 5))
hist(rgamma(N, 2, 1))
PTS <- rnorm(N, rnorm(N, mean = 30, sd = 5), rgamma(N, 2, 1))
hist(PTS)


# Test the model with simulated (fake) data, to see if the model works
# by correctly updating the priors and if it makes sense
m <- alist(
  #likelihood 
  pts ~ dnorm(mu, sigma),
  
  #priors
  mu ~ dnorm(30, 5),
  sigma ~ dgamma(2,1)
  
)

#Simulated data 
N_games <- 1e3
mu <- 30
sd <- 5
sim_pts <- data.frame( pts = round(rnorm(N_games, mu, sd), 0) )

# This creates a model object by quadratic approximation 
m_sim_fit <- quap(m, data=sim_pts)

m_sim_fit
m_sim_fit@formula
m_sim_fit@start
m_sim_fit@coef
m_sim_fit@data


#Now we go to the real data set and fit the model with the priors i specified
shaq <- read_csv("data/shaq.csv")
