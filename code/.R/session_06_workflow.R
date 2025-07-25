library(ggplot2)

# priors  -----------------------------------------------------

# uniform 

x_min <- -5
x_max <- 5
range <- seq(x_min, x_max, length.out = 100) # sample space
#range
d <- dunif(range, min = x_min, max = x_max) # densities
UNI <- data.frame(range, d)

ggplot(UNI, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff") +
  labs(x = "x", 
       y = "Density") 

# normal  

x_min <- -5
x_max <- 5
range <- seq(x_min, x_max, length.out = 100) # range
d <- dnorm(range, mean = 0, sd = 1) # densities
NORM <- data.frame(range, d)

ggplot(NORM, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff") +
  labs(x = expression(mu_j),
       y = "Density") 


# beta

range <- seq(0, 1, length.out = 100)

d <- dbeta(range, shape1 = 50, shape2 = 1)
BETA <- data.frame(range, d)

ggplot(BETA, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff" ) +
  labs(x = "x", 
       y = "Density")


# exponential

x_min <- 0
x_max <- 10 
range <- seq(x_min, x_max, length.out = 100)
d <- dexp(range, rate = 1)
EXP <- data.frame(range, d)

ggplot(EXP, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff") +
  labs(x = "x", 
       y = "Density") 


# gamma 

x_min <- 0
x_max <- 10
range <- seq(x_min, x_max, length.out = 100)
d <- dgamma(range, shape = 2, rate = 1)
GAMMA <- data.frame(range, d)

ggplot(GAMMA, aes(x = range, y = d)) +
  geom_line(linewidth = 2, color = "#ff02ff") +
  labs(x = "x", 
       y = "Density") 



# Prior predictive simulation --------------------------------------------------

# specify prior 

a <- 2
b <- 5

theta <- seq(0,1, length.out = 1e3)
d <- dbeta(theta, shape1 = a, shape2 = b)
summary <- data.frame(theta, d)

ggplot(summary, aes(x = theta, y = d)) +
  geom_line(size = 1, linetype = "dashed", color = "#ff02ff" ) +
  labs(x = expression(theta), 
       y = "Density") 

# sample from prior
no <- 1e3
prior_smp <- data.frame(smp = rbeta(no, a, b))
prior_smp

ggplot(summary) +
  geom_line(size = 1, linetype = "dashed",  color = "#ff02ff", aes(x = theta, y = d)) +
  geom_density(data = prior_smp, aes(x = smp), color = "green", size = 1) + 
  labs(x = expression(theta), 
       y = "Density")



preds <- data.frame(L =vector("numeric", nrow(prior_smp)))

N <- 1e3

set.seed(832)

for (i in seq_along(prior_smp$smp)){ 
  
  preds[i, "L"] <- rbinom(n = 1, size = N, prob = prior_smp[i, "smp"])
  
}

preds %>% ggplot(aes(x=L)) + 
  geom_histogram(fill = "green", color = "green", 
                 alpha = .5, bins = 100) + 
  scale_x_continuous(limits = c(0,N), breaks = seq(0,N,100)) + 
  labs(x = "Number of Simulated L out of 1000",
       y = "Frequency") 

# Testing the model ----------------------------------------------

# scale prior to probability 
summary$prior <- ((summary$d)/sum(summary$d))
#upper <- seq((1/1e3), 1, length.out = 1e3)
#lower <- seq(0, (999/1e3), length.out = 1e3)
#summary$prior <- (pbeta(upper, 2, 5) - pbeta(lower,2,5))

ggplot(summary, aes(x = theta, y = prior)) +
  geom_line(size = 1, linetype = "dashed") +
  labs(x = expression(theta), 
       y = "Prior")

# simulate data
sim_toss <- function(N, p){
  sample(c("L", "W"), size=N, replace=TRUE, prob=c(p, 1-p)) 
}

N <- 1e3
set.seed(12385)
obs <- sim_toss(N, p = .3)

# grid approximation of posterior

compute_post <- function(obs, summary){ 
  L <- sum(obs=="L")
  likelihood <- dbinom(L, N, prob = summary$theta)
  posterior <- likelihood*summary$prior
  posterior_norm <- posterior/sum(posterior)
  tibble(summary,lh=round(likelihood, 3), post=round(posterior_norm,3))
}
estimation <- compute_post(obs, summary)

# Check results
estimation %>% 
  pivot_longer(cols = c(prior,post), 
               names_to = "type", 
               values_to = "probability") %>% 
  ggplot(aes(x=theta, y = probability, color = type, linetype = type)) + 
  scale_color_manual(values = c( "green", "#ff02ff")) +
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(x = "Theta", 
       y = "Probability", 
       color = "Probability",
       linetype = "Probability")


# posterior predictive check ----------------------------------------------

estimation %>% 
  ggplot(aes(x=theta, y = post)) + 
  geom_line(size = 1, linetype = "dashed", color = "#ff02ff" ) + 
  labs(x = expression(theta), 
       y = "Probability") 

set.seed(123461)
post_smp <- data.frame(smp = sample(estimation$theta, 1e3, prob = estimation$post, replace = TRUE))

ggplot(post_smp, aes(x=smp)) +
  geom_density(color = "green") + 
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) + 
  labs(x = expression(theta), 
       y = "Density") 

post_preds <- data.frame(L =vector("numeric", nrow(post_smp)))

N <- 1e3
for (i in seq_along(prior_smp$smp)){ 
  
  post_preds[i, "L"] <- rbinom(n = 1, size = N, prob = post_smp[i, "smp"])
  
}

post_preds %>% ggplot(aes(x=L)) + 
  geom_histogram(fill = "green", color = "green", alpha = .5, bins = 100) + 
  scale_x_continuous(limits = c(0,N), breaks = seq(0,N,100)) + 
  labs(x = "Number of Simulated L out of 1000",
       y = "Frequency") 


# NYC Flights Exercise ----------------------------------------------------

# install and load packages
library(nycflights13)
#library(nycflights23)

# Step 1: Define prior 

# define possible parameter values (between 0 and 1 because we are estimating a probability)
candidates <- seq(0,1,.01)  

# prior plausibility for each candidate -- I use Beta(4,2) as I think that most flights will arrive on time
prior <- dbeta(candidates, shape1=4, shape2=2)

# plot prior
plot(x=candidates, y=prior, type = "l")


# Step 2: Prepare data

## store flights dataset as an object 'dat'
dat <- nycflights13::flights
#dat <- nycflights23::flights

## remove all columns that have a missing (NA) in columns arr_delay
dat <- dat[!is.na(dat$arr_delay), ]

## add binary variable (ontime), indicating whether delay was less (ontime=1) or more than 10 minutes (ontime=0) 
dat$ontime <- ifelse(dat$arr_delay < 10, 1, 0)

## compute total number of flights that were on time
total <- sum(dat$ontime)

# Step 3: Inference

## compute likelhood
lh <- dbinom(x=total, size=nrow(dat), prob=candidates)

## compute posterior
post <- lh*prior/sum(lh*prior)

## plot results
plot(x=candidates, y=post, type = "l", ylim=c(0,1)) # posterior
lines(x=candidates, y=prior/sum(prior), col='red') # prior


# separately for each month ---------------------------------------------------------

# Step 1: Compute posteriors

## prepare empty matrix to store posterior values for each candidate (rows), separately for each month (column)
posts <- matrix(nrow = length(candidates), ncol = 12)

## Compute posteriors for monthly arrival  data

for(i in 1:12){
  
  # get data from respective month
  dat_month <- dat[dat$month==i , ] 
  total_month <- sum(dat_month$ontime) 
  
  # Bayesian inference
  lh_month <- dbinom(x=total_month, size=nrow(dat_month), prob=candidates) #  likelihood
  post_month <- lh_month*prior/sum(lh_month*prior) # post
  
  # store data in respective column
  posts[,i] <- post_month
  
}


# Step 2: Show results

## load additional packages

library(tidyverse) # packages/functions for data wrangling and visualizations
library(viridis) # color palettes

## prepare data frame
results <- 
  # merge candidates and matrix with posteriors into one data frame
  data.frame(candidates, posts) %>% 
  # pivot the data frame into a long format (month and posterior column instead of separate posterior columns for each month) 
  pivot_longer(cols = X1:X12, names_to = "month", values_to = "post", names_prefix = "X")

## plot data

results %>% 
  # change month column to numeric (allows automatic ordering of months in the graph) 
  mutate(month=as.numeric(month)) %>% 
  ggplot(aes(x=candidates, y=post, group=month, color=month, fill=month)) +
  facet_wrap(~month, nrow=12) +
  geom_line(linewidth=1.5, alpha=.5) +
  scale_y_continuous(breaks = seq(0,1,.5)) +
  labs(x='p in Binomial(N,p)', 
       y='Posterior',
       color='Month') + 
  theme_minimal() + 
  scale_color_viridis(option = 'D')

