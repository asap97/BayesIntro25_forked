#If there are some candidates not included in the analysis will not 
#matter at all. Bayes always shifts the probability over candidates 
#considered at the beginning 

#Correlation means information of one factor/variable is already included in 
#another one

#Write a function that simulates the input data
#n is number of tosses, p is probability of land
simu <- function(n = 10, p = 0.3) {
  ifelse(runif(n) < p, "L", "W")
}
#ex <- simu(n = 1000, p = 0.45)
#sum(ex == "L")/length(ex)


#The prior should express your uncertainty BEFORE seeing the data, and then after
#looking at the data, it can be adjusted. Prior knowledge means putting more 
#weight on more plausible candidates that we KNOW OF 

#With more data, the prior matters less because it tends to be guided 
#mostly by the data. If you have only few points of data, it makes sense to 
#rely on priors.


# Counter function that returns probabilities, either by finding the sides, or just the 
# normal function withouth the sides, just the cp
counter_prob <- function(data, cp){ 
  sides <- length(cp) - 1
  L <- sum(data == "L")
  W <- sum(data == "W")
  ways <- (cp * sides)^L * ((1 - cp) * sides)^W 
  probs <- ways / sum(ways)
  data.frame(cp, probs)
}
counter_prob_1 <- function(data, cp){
  L <- sum(data == "L")
  W <- sum(data == "W")
  probs <- (cp)^L * ((1 - cp))^W
  probs <- probs/sum(probs)
  data.frame(cp, probs)
}



cp_seq <- seq(0, 1, 0.1)


# Initial data (prior knowledge) with give proportion of land
old_data <- c("W", "L")
#old_data <- simu(n=10, p=0.15)
prior_prob <- counter_prob_1(old_data, cp = cp_seq)
#results <- round(prior_prob, 3)
results <- prior_prob
colnames(results) <- c("cp_Land","prior")
#results$prior <- round(results$prior, 3)
#sum(results$prior)
results

# Simulate new data point(s) with a given probability to have land
new_data <- simu(n = 10, p = 0.3)
data_prob <- counter_prob_1(new_data, cp = cp_seq)
results$likelihood <- data_prob$probs
#results$likelihood <- round(data_prob$probs, 3)
#sum(results$likelihood)


# Combine data for new count
all_data <- c(old_data, new_data)
post_prob <- counter_prob_1(all_data, cp = cp_seq)
results$posterior <- post_prob$probs
#results$posterior <- round(post_prob$probs, 3)
#sum(results$posterior)

#Check that post_prob is correct 
results$post_check <- (results$prior*results$likelihood)/(sum(results$prior*results$likelihood))
results$post_check <- round(results$post_check, 3)


results <- round(results, 3)
results

#Priors force you to think a little bit more about your model and specify 
#it, why are the priors set as they are, and what things are you maybe leaving out
#and why

#Loading libraries to plot
library(ggplot2)
library(tidyr)

# Convert data to long format for plotting
plot_data <- pivot_longer(results, cols = c("prior", "posterior"),
                          names_to = "Type", values_to = "Probability")

# Line plot: dotted for prior, solid for posterior
ggplot(plot_data, aes(x = cp_Land, y = Probability, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_linetype_manual(values = c("prior" = "dotted", "posterior" = "solid")) +
  labs(title = "Prior vs Posterior Probabilities",
       x = "Probability of Land (cp)",
       y = "Probability") +
  theme_minimal()



# Sampling and finding mean, sd, mode and 89%-PI and -DHPI of the posterior
results
s <- sample(cp_seq, 1e3, p=results$posterior, replace = TRUE)
mean(s)
sd(s)

#function to get the mode 
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode(s)

#calculating the intervals
percent <- 0.85
pi_bounds <- quantile(s, probs = c((1 - percent)/2, 1 - (1 - percent)/2))
library(HDInterval)
hdi_interval<- hdi(s, credMass = percent)

