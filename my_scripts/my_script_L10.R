#The logit is the inverse of the logistic and vice versa, when transformed to the
#logistic regression, the coefficients show the change on predicted probability
#for a unitary change in the predictor variable

#Exercise 1 
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


