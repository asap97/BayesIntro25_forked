library(rethinking)

N <- 1e3
G <- rbinom(N, 1, prob=.5)+1 # determine group membership
Q <- rnorm(N, mean=ifelse(G==1, .35, .80), sd=0.1) # determine quality
simu <- data.frame(G=G, Q=Q)

#-----Pooling model (all data)------
m1_pooling <- ulam(
  alist(
    # likelihood
    Q ~ dnorm(mu, sigma),
    
    #priors
    mu ~ dnorm(.5, .2) , 
    sigma ~ dnorm(.3, .05)
    
  ), 
  data = simu ,
  chains = 4, 
  cores = 4, 
  log_lik = TRUE
)
traceplot(m1_pooling)
precis(m1_pooling)
mean(Q)
sd(Q)

#-----No pooling - index model------
m1_no_pooling <-ulam(
  alist(
    #likelihood 
    Q ~ dnorm(mu, sigma),
    mu <- a[G],
    
    #priors 
    a[G] ~ dnorm(0.5,0.1),
    sigma ~ dnorm(.3, .05)
    
  ), 
  data = simu ,
  chains = 4, 
  cores = 4, 
  log_lik = TRUE
)
traceplot(m1_no_pooling)
precis(m1_no_pooling, depth=2)


#-----Partial pooling - hyper distribution------
m1_partial <- ulam(
  alist(
    #likelihood 
    Q ~ dnorm(mu, sigma),
    mu <- a[G],
    
    #priors 
    a[G] ~ dnorm(k,tau),
    k ~ dnorm(0.5,0.2),
    tau ~ dnorm(0,0.1),
    sigma ~ dnorm(.3, .05)
    
  ), 
  data = simu ,
  chains = 4, 
  cores = 4, 
  log_lik = TRUE
)
traceplot(m1_partial)
precis(m1_partial, depth=4)

compare(m1_no_pooling, m1_pooling, m1_partial)
compare(m1_no_pooling, m1_partial)
