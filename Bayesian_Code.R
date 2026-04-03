# ==========================================
# MODULE 3: BAYESIAN INFERENCE
# ==========================================

# ----------------------------------------------------
# Problem 1 (Exercise 4.64)
# ----------------------------------------------------
# Plot MSE of ML estimator vs Bayesian estimator
MSE_ML <- function(n, pi_val) {
  return((pi_val * (1 - pi_val)) / n)
}

MSE_Bayes <- function(n, pi_val) {
  return((n * pi_val * (1 - pi_val) + (1 - 2 * pi_val)^2) / (n + 2)^2)
}

pi_seq <- seq(0, 1, length.out=400)

pdf("Bayesian_prob1_plot.pdf", width=10, height=5)
par(mfrow=c(1,2))

# n = 10
plot(pi_seq, MSE_ML(10, pi_seq), type="l", col="blue", lwd=2, ylim=c(0, 0.05),
     xlab=expression(pi), ylab="MSE", main="MSE for n = 10")
lines(pi_seq, MSE_Bayes(10, pi_seq), col="red", lwd=2, lty=2)
legend("topleft", legend=c("ML Estimator", "Bayes Estimator"), col=c("blue", "red"), lwd=2, lty=c(1, 2))

# n = 1000
plot(pi_seq, MSE_ML(1000, pi_seq), type="l", col="blue", lwd=2, ylim=c(0, 0.0005),
     xlab=expression(pi), ylab="MSE", main="MSE for n = 1000")
lines(pi_seq, MSE_Bayes(1000, pi_seq), col="red", lwd=2, lty=2)
legend("topleft", legend=c("ML Estimator", "Bayes Estimator"), col=c("blue", "red"), lwd=2, lty=c(1, 2))
dev.off()


# ----------------------------------------------------
# Problem 2 (Exercise 4.66)
# ----------------------------------------------------
# Suppose mu = sigma = n = 1
# Bias = (c - 1) * mu 
# Var = c^2 * sigma^2 / n
# MSE = Bias^2 + Var

mu <- 1
sigma <- 1
n <- 1

c_seq <- seq(0, 1, length.out=100)
bias_sq <- ((c_seq - 1) * mu)^2
var_tilde <- (c_seq^2 * sigma^2) / n
mse_tilde <- bias_sq + var_tilde

pdf("Bayesian_prob2_plot.pdf", width=6, height=5)
plot(c_seq, mse_tilde, type="l", col="black", lwd=2, ylim=c(0, 1.2),
     xlab="c", ylab="Value", main="Bias-Variance Tradeoff")
lines(c_seq, bias_sq, col="red", lwd=2, lty=2)
lines(c_seq, var_tilde, col="blue", lwd=2, lty=3)
legend("top", legend=c("MSE", "Bias^2", "Variance"), col=c("black", "red", "blue"), lwd=2, lty=c(1, 2, 3))
dev.off()

# c to minimize MSE:
c_min <- c_seq[which.min(mse_tilde)]
cat("Empirical c that minimizes MSE:", c_min, "\n")
cat("Theoretical c = n*mu^2 / (n*mu^2 + sigma^2) = 1 / 2 = 0.5\n")

cat("Bayesian Module R code executed successfully.\n")
