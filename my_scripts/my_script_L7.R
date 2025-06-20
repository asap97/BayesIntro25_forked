# load packages
library(tidyverse)
library(rethinking)
library(ggplot2)

#distribution parameters
N <- 1e3
a <- 2
b <-4

# Repeat and then plot
hist(replicate(N, sum(rbeta(N, a, b))))
hist(replicate(N, sum(rgamma(N, a, b))))
hist(replicate(N, sum(runif(N, a, b))))



# Priors for the model PTS_i ~ N(mu, sigma)
# For the mean of points scored in a game a normal distribution with mean 30
# and sd of 5, for the sd of points per game gamma bc it shouldnt take 
# negative values
hist(rnorm(N, mean = 30, sd = 5))
hist(rgamma(N, 2, 0.08))
PTS <- rnorm(N, rnorm(N, mean = 30, sd = 5), rgamma(N, 2, 1))
hist(PTS)


# Test the model with simulated (fake) data, to see if the model works
# by correctly updating the priors and if it makes sense
m <- alist(
  #likelihood 
  # this name of the variable has to be the same in the data set 
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
#m_sim_fit@data


#Now we go to the real data set and fit the model with the priors i specified
shaq <- read_csv("data/shaq.csv")
N_games <- nrow(shaq)
real_pts <- data.frame(pts=shaq$PTS)

m_real_fit <- quap(m, data=real_pts)

m_real_fit
m_real_fit@formula
m_real_fit@start
m_real_fit@coef
#m_real_fit@data
precis(m_real_fit)
pairs(m_real_fit, pars = c("mu", "sigma"))

post_pred <- extract.samples(m_real_fit, n=1e3)
head(post_pred)
#hist(rnorm(n=1000, post_pred$mu, post_pred$sigma))
#hist(real_pts$pts)

# Plotting it better and against a simulated with the posterior
N_games <- 1e3
mu <- m_real_fit@coef[1]
sd <- m_real_fit@coef[2]
#sim_pts_prior <- sim_pts
sim_pts <- data.frame( pts = round(rnorm(N_games, mu, sd), 0) )

# Label the data
sim_pts_labeled   <- sim_pts       %>% mutate(source = "Posterior")
#prior_pts_labeled <- sim_pts_prior %>% mutate(source = "Prior")
real_pts_labeled  <- real_pts      %>% mutate(source = "Real")

all_pts <- bind_rows(sim_pts_labeled, real_pts_labeled)
#all_pts <- bind_rows(sim_pts_labeled, prior_pts_labeled, real_pts_labeled)

# Plot
ggplot(all_pts, aes(x = pts, fill = source)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  scale_fill_manual(values = c(
    "Posterior" = "#C71585", 
    #"Prior"     = "#FFA500", 
    "Real"      = "#87CEEB"
    
  )) +
  labs(x = "Points", y = "Count", fill = "Data Source") +
  theme_minimal()



#-----------------------INCLUDING PREDICTORS------------------
# When including a predictor, you condition the prediction. For example, you 
# can include field goal attempts FGA as x_i. Beta*FGA needs to have points as 
# its unit, so beta is how many points you can get for a field goal attempt. 
# You can get between 0 and 3 points in basketball --> You should to know
# something about the values a predictor can take 


