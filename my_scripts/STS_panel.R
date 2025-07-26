library(tidyverse)
library(ggplot2)

a <- 7
b <- 20

theta <- seq(0,1, length.out = 1e4)
d <- dbeta(theta, shape1 = a, shape2 = b)
summary <- data.frame(theta, d)

# sample from prior
no <- 1e5
sample <- data.frame(smp = rbeta(no, a, b))

# Plot
ggplot(summary) +
  geom_density(data = sample, aes(x = smp), color = "black", fill = NA, linewidth = 1.5) +
  labs(
    x = "Proportion of land",
    y = "Density",
    title = "Prior Distribution of Proportion of Land"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.4, face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )


# Step 1: Prior setup using bins of 0.05
prop <- seq(0, 1, length.out = 21)  # 0.00, 0.05, ..., 1.00
priors <- vector("numeric", length(prop))

for (i in seq_along(prop)) {
  priors[i] <- round(sum(sample >= prop[i] & sample < prop[i + 1]) / 1e4, 2)
}
poss <- tibble(prop_L = seq(0, 0.95, 0.05), prior = priors[1:20])

# Step 2: Sample from prior
N <- 1e3
sample <- sample(poss$prop_L, size = N, replace = TRUE, prob = poss$prior)
sample <- data.frame(smp = sample)

# Step 3: Predict outcomes using prior
preds <- data.frame(L = rbinom(N, size = 100, prob = sample$smp))

# Step 4: Prior histogram (clean and Canva-ready)
ggplot(preds, aes(x = L)) +
  geom_histogram(fill = "grey60", color = "grey30", bins = 30) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "# of Land observations (out of 100)",
    y = "Frequency",
    title = "Predictions from Prior"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Step 5: Update posterior with 26 land observations
lh <- dbinom(26, 100, prob = poss$prop_L)
posterior <- poss$prior * lh
posterior_norm <- posterior / sum(posterior)

poss$likelihood <- lh
poss$posterior <- posterior_norm
poss[, -(1:2)] <- round(poss[, -(1:2)], 4)

# Step 6: Sample from posterior
post_sample <- sample(poss$prop_L, size = N, replace = TRUE, prob = poss$posterior)
post_sample <- data.frame(smp = post_sample)

# Step 7: Predict outcomes using posterior
preds_post <- data.frame(L = rbinom(N, size = 100, prob = post_sample$smp))

# Step 8: Posterior histogram
ggplot(preds_post, aes(x = L)) +
  geom_histogram(fill = "grey60", color = "grey30", bins = 30) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "# of Land observations (out of 100)",
    y = "Frequency",
    title = "Predictions from Posterior"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Step 9: Report 95% posterior interval for proportion of land
ci_95 <- quantile(post_sample$smp, probs = c(0.025, 0.975))
cat("95% posterior interval for proportion of land: [",
    round(ci_95[1], 2), ", ", round(ci_95[2], 2), "]\n")
