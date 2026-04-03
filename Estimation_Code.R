# ==========================================
# MODULE 1: ESTIMATION
# ==========================================

# ----------------------------------------------------
# Problem 1 (Exercise 4.33)
# ----------------------------------------------------
set.seed(123)
n <- 100
n_sim <- 100000

means <- numeric(n_sim)
medians <- numeric(n_sim)

for (i in 1:n_sim) {
  sample_data <- rnorm(n, mean = 0, sd = 1)
  means[i] <- mean(sample_data)
  medians[i] <- median(sample_data)
}

# Plot sampling distributions
pdf("Estimation_prob1_plot.pdf", width=8, height=5)
par(mfrow=c(1,2))
hist(means, breaks=50, col="skyblue", main="Sampling Dist of Mean", xlab="Sample Mean", prob=TRUE)
hist(medians, breaks=50, col="lightgreen", main="Sampling Dist of Median", xlab="Sample Median", prob=TRUE)
dev.off()

cat("MSE of Sample Mean around 0: ", mean(means^2), "\n")
cat("MSE of Sample Median around 0: ", mean(medians^2), "\n")
cat("The mean has a lower MSE, so it is a better estimator for the normal distribution mean/median.\n")

# ----------------------------------------------------
# Problem 2 (Exercise 4.34)
# ----------------------------------------------------
# For a uniform distribution, say U(0, 1) where true mean=0.5, true median=0.5
set.seed(123)
n <- 100
n_sim <- 100000

means_unif <- numeric(n_sim)
medians_unif <- numeric(n_sim)

for (i in 1:n_sim) {
  sample_data <- runif(n, min = 0, max = 1)
  means_unif[i] <- mean(sample_data)
  medians_unif[i] <- median(sample_data)
}

mse_mean_unif <- mean((means_unif - 0.5)^2)
mse_median_unif <- mean((medians_unif - 0.5)^2)

cat("MSE of Sample Mean (Uniform) around 0.5: ", mse_mean_unif, "\n")
cat("MSE of Sample Median (Uniform) around 0.5: ", mse_median_unif, "\n")
cat("For the uniform distribution, the mean has a lower MSE than the median, making it a better estimator.\n")

# ----------------------------------------------------
# Problem 3 (Exercise 4.36)
# ----------------------------------------------------
# Analytical problem. Proof will be provided in LaTeX.

# ----------------------------------------------------
# Problem 4 (Exercise 4.39)
# ----------------------------------------------------
# (c) Plot L(lambda) - L(lambda_hat) for lambda between 0.01 and 4.00
# y_bar = 10, so lambda_hat = 1/y_bar = 0.1
# L(lambda) = n * ln(lambda) - lambda * sum(y) = n * ln(lambda) - lambda * n * y_bar
# L(lambda_hat) = n * ln(lambda_hat) - lambda_hat * n * y_bar

y_bar <- 10
lambda_hat <- 1 / y_bar

lambda_seq <- seq(0.01, 4.00, length.out = 400)

log_lik_diff <- function(n, lambda, y_bar, lambda_hat) {
  L_lambda <- n * log(lambda) - lambda * n * y_bar
  L_lambda_hat <- n * log(lambda_hat) - lambda_hat * n * y_bar
  return(L_lambda - L_lambda_hat)
}

pdf("Estimation_prob4_plot.pdf", width=8, height=6)
plot(lambda_seq, log_lik_diff(1, lambda_seq, y_bar, lambda_hat), type="l", col="blue", lwd=2,
     ylab="L(lambda) - L(lambda_hat)", xlab="lambda", main="Relative Log-Likelihood")
lines(lambda_seq, log_lik_diff(5, lambda_seq, y_bar, lambda_hat), type="l", col="red", lwd=2)
lines(lambda_seq, log_lik_diff(10, lambda_seq, y_bar, lambda_hat), type="l", col="green", lwd=2)
legend("topright", legend=c("n = 1", "n = 5", "n = 10"), col=c("blue", "red", "green"), lwd=2)
abline(h=0, lty=2)
abline(v=lambda_hat, lty=2)
dev.off()

cat("Estimation Module R code executed successfully.\n")
