#The logit is the inverse of the logistic and vice versa, when transformed to the
#logistic regression, the coefficients show the change on predicted probability
#for a unitary change in the predictor variable

#--------------------Exercise 1------------------------ 
library(ggplot2)
#Assuming that alpha follows a normal distribution with mean 0
sd <- 10
samp <- rnorm(300, 0, sd)
theta <- exp(samp)/(1 + exp(samp))
df <- data.frame(
  samp = samp,
  theta = theta,
  density = dnorm(samp, mean = 0, sd = sd)
)

# Create data frame for ggplot
df <- data.frame(samp = samp, theta = theta)

# Scatter plot of samp vs theta
ggplot(df, aes(x = samp, y = theta)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") +
  labs(x = "Sample", y = expression(theta)) +
  theme_minimal()

ggplot(df, aes(x = theta)) +
  geom_histogram(fill = "purple", color = "white", bins = 30) +
  labs(x = "Sample", y = "Count") +
  theme_minimal()



#--------------------Exercise 2-----------------------
library(rethinking)
library(tidyverse)
data("UCBadmit", "UCBadmit_long")
#With the aggregate data you use the binomial, in the individual one you 
#use the Bernoulli. When possible, aggregating is might make the model run 
#faster
dat_wide <- UCBadmit
dat_long <- UCBadmit_long

#Modifying the individual data to be able to work with it
dat_long <- list(
  A = dat_long$admit,
  G = ifelse(dat_long$applicant.gender=="female", 1, 2),
  D = as.integer(dat_long$dept)
)
view(dat_long)

#Modifying the aggregate data to be able to work with it
dat_wide <- list(
  A = dat_wide$admit , 
  N = dat_wide$applications , 
  G = ifelse(dat_wide$applicant.gender=="female", 1, 2) , 
  D = as.integer(dat_wide$dept)
)
view(dat_wide)


#MAIN RQ: Was there gender discrimination?

# estimate total effect of G (ignore D), using the AGGREGATED data
m1 <- ulam(
  alist(
    # likelihood
    A ~ dbinom(N, p) , 
    logit(p) <- a[G]  , 
    
    # priors
    a[G] ~ dnorm(0,1) 
  ) , 
  data=dat_wide, 
  chains = 4, 
  cores = 4, 
  file = "my_scripts/my_models/session_10_m1" # loads model fit file when previously fitted and stored
)
precis(m1, depth = 2)
inv_logit(coef(m1))
traceplot(m1)


# estimate total effect of G (ignore D), using the INDIVIDUAL data
m2 <- ulam(
  alist(
    # likelihood
    A ~ dbern(p) , 
    logit(p) <- a[G]  , 
    
    # priors
    a[G] ~ dnorm(0,1) 
  ) , 
  data=dat_long, 
  chains = 4, 
  cores = 4,
  file = "my_scripts/my_models/session_10_m2" # loads model fit file when previously fitted and stored
)
precis(m2, depth = 2)
inv_logit(coef(m2))
traceplot(m2)



# estimate total effect of G CONSIDERING D, using the AGGREGATED data
m3 <- ulam(
  alist(
    # likelihood
    A ~ dbinom(N, p) , 
    logit(p) <- a[G, D]  , 
    
    # priors
    matrix[G, D]:a ~ dnorm(0,1) 
  ) , 
  data=dat_wide, 
  chains = 4, 
  cores = 4, 
  file = "my_scripts/my_models/session_10_m3" # loads model fit file when previously fitted and stored
)
precis(m3, depth = 3)
inv_logit(coef(m3))
traceplot(m3)


#FOR THE ASSIGNMENT
#logit(p) = a[G,D]
#logit(p) = a[D]+b[D]G
#dep <- paste0("D", 1:6)


