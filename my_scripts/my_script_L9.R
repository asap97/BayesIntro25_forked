days <- 1000*7
islands <- data.frame(island = c(1:10), popu = sample(1:500, 10, replace=FALSE),
                      visits = rep(0, 10))
current <- sample(1:10, 1)


for (day in c(1:days)){
  #add a visit to the current position
  islands[current,"visits"] <- islands[current,"visits"]+1
  #generate a proposal island
  proposal <- sample(c(1,-1), 1) + current
  #print(proposal)
  #make sure the proposal makes sense if it is at an "edge"
  if (proposal == 0){
    proposal <- 10
  }else if(proposal == 11){
    proposal <- 1
  }
  #calculate the ratio 
  ratio <- islands[proposal,"popu"]/islands[current,"popu"]
  #print(ratio)
  #update the island based on the ratio
  if(ratio>runif(1)){
    current <- proposal
  }
}
  
barplot(t(as.matrix(islands[, c("popu", "visits")])),
        beside = TRUE,
        col = c("skyblue", "orange"),
        names.arg = 1:nrow(islands),
        legend.text = c("Population", "Visits"),
        args.legend = list(x = "topright"),
        main = "Comparison of Population vs Visits per Island",
        xlab = "Island",
        ylab = "Count")


#-----------------------Using ulam() for MCMC------------------------


library(rethinking)
library(parallel)
library(here)

shaq <- read.csv(here("data", "shaq.csv"))
dat <- list( 
  N = nrow(shaq) , 
  pts = shaq$PTS ,
  min = shaq$Minutes ,
  fga = shaq$FGA , 
  fta = shaq$FTA , 
  min_bar = round(mean(shaq$Minutes), 2) , 
  fga_bar = round(mean(shaq$FGA), 2) ,
  fta_bar = round(mean(shaq$FTA), 2)
)


# use ulam (interfacec to Stans HMC Sampler)
cores <- detectCores(logical = TRUE)
n_chains <- 3

mshaq <- ulam(
  alist(
    
    # likelihood
    pts ~ dnorm(mu, sigma), 
    mu <- a + b_1 * (min - min_bar) + b_2 * (fga - fga_bar) * 2 + b_3 * (fta - fta_bar),
    
    # prior
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2), 
    b_2 ~ dunif(0, 2), 
    b_3 ~ dunif(0, 1), 
    sigma ~ dunif(0,10)
  ),
  data = dat, 
  chains = n_chains , # number of independent MCMC chains 
  cores = n_chains , # number of cores that 
  iter = 300)

precis(mshaq)
rethinking::traceplot(mshaq)
stancode(mshaq)

