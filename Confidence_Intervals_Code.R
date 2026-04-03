# ==========================================
# MODULE 2: CONFIDENCE INTERVALS
# ==========================================

# ----------------------------------------------------
# Problem 1 (Exercise 4.12)
# ----------------------------------------------------
income_data <- read.table("Income.dat", header=TRUE)

# (a) Boxplot
pdf("CI_prob1_boxplot.pdf", width=6, height=5)
boxplot(income ~ race, data=income_data, 
        main="Annual Incomes by Race", 
        ylab="Income (Thousands of $)", 
        xlab="Race", col=c("lightblue", "lightgreen", "lightpink"))
dev.off()

# Subset data for Blacks and Whites
income_B <- income_data$income[income_data$race == "B"]
income_W <- income_data$income[income_data$race == "W"]

# (b) 90% CI for diff between means (equal variance)
# White vs Black (W-B)
t_test_equal_var <- t.test(income_W, income_B, var.equal=TRUE, conf.level=0.90)
cat("90% CI (Equal Variance) for W - B:\n")
print(t_test_equal_var$conf.int)

# (c) 90% CI for diff between means (unequal variance)
t_test_unequal_var <- t.test(income_W, income_B, var.equal=FALSE, conf.level=0.90)
cat("90% CI (Unequal Variance) for W - B:\n")
print(t_test_unequal_var$conf.int)


# ----------------------------------------------------
# Problem 2 (Exercise 4.16)
# ----------------------------------------------------
substance_data <- read.table("Substance.dat", header=TRUE)

# Aggregate counts based on Alcohol and Marijuana usage
agg_data <- aggregate(count ~ alcohol + marijuana, data=substance_data, FUN=sum)

# Total with Alcohol=yes
n_alc_yes <- sum(agg_data$count[agg_data$alcohol == "yes"])
# Total with Alcohol=yes and Marijuana=yes
m_alc_yes <- sum(agg_data$count[agg_data$alcohol == "yes" & agg_data$marijuana == "yes"])

# Total with Alcohol=no
n_alc_no <- sum(agg_data$count[agg_data$alcohol == "no"])
# Total with Alcohol=no and Marijuana=yes
m_alc_no <- sum(agg_data$count[agg_data$alcohol == "no" & agg_data$marijuana == "yes"])

p1 <- m_alc_yes / n_alc_yes
p2 <- m_alc_no / n_alc_no

# (a) Wald CI formula
z <- 1.96
se <- sqrt(p1*(1-p1)/n_alc_yes + p2*(1-p2)/n_alc_no)
ci_lower <- (p1 - p2) - z * se
ci_upper <- (p1 - p2) + z * se
cat("\n95% Wald CI using formula:\n")
cat("(", ci_lower, ",", ci_upper, ")\n")

# (b) Using software (prop.test without continuity correction closely matches Wald)
cat("\n95% CI using prop.test:\n")
prop_test_res <- prop.test(c(m_alc_yes, m_alc_no), c(n_alc_yes, n_alc_no), correct=FALSE)
print(prop_test_res$conf.int)


# ----------------------------------------------------
# Problem 3 (Exercise 4.17)
# ----------------------------------------------------
set.seed(42)
t_data <- rt(10000, df=3)

pdf("CI_prob3_plots.pdf", width=10, height=5)
par(mfrow=c(1,2))
hist(t_data, breaks=100, main="Histogram of t(3) data", xlab="Value", col="cyan")
qqnorm(t_data, main="Normal Quantile Plot")
qqline(t_data, col="red", lwd=2)
dev.off()


# ----------------------------------------------------
# Problem 5 (Exercise 4.50)
# ----------------------------------------------------
pi_true <- 0.06
n <- 20
z <- 1.96

# Function to compute Wald CI correctly capturing 0
wald_ci <- function(successes, n) {
  p_hat <- successes / n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  lower <- p_hat - z * se
  upper <- p_hat + z * se
  return(c(lower, upper))
}

# (a) First 10 samples
set.seed(123)
cat("\n--- Problem 5: Simulation ---\n")
samples_10a <- rbinom(10, n, pi_true)
wald_contains_a <- sum(sapply(samples_10a, function(x) {
  ci <- wald_ci(x, n)
  ci[1] <= pi_true && ci[2] >= pi_true
}))
cat("First 10 samples Wald coverage count:", wald_contains_a, "\n")

# Next 10 samples
samples_10b <- rbinom(10, n, pi_true)
wald_contains_b <- sum(sapply(samples_10b, function(x) {
  ci <- wald_ci(x, n)
  ci[1] <= pi_true && ci[2] >= pi_true
}))
cat("Second 10 samples Wald coverage count:", wald_contains_b, "\n")

# 1000 samples
samples_1000 <- rbinom(1000, n, pi_true)
wald_contains_1000 <- sum(sapply(samples_1000, function(x) {
  ci <- wald_ci(x, n)
  ci[1] <= pi_true && ci[2] >= pi_true
}))
cat("1000 samples Wald coverage percentage:", (wald_contains_1000 / 1000) * 100, "%\n")

# (b) Sampling distribution of p_hat for 10000 samples
samples_10000 <- rbinom(10000, n, pi_true)
p_hat_dist <- samples_10000 / n

pdf("CI_prob5_phat_dist.pdf", width=6, height=5)
hist(p_hat_dist, breaks=seq(-0.025, 1.025, by=0.05), col="orchid", 
     main="Sampling Dist of p_hat (n=20, pi=0.06)", xlab="p_hat", prob=TRUE)
dev.off()

# (c) Analytical. When p_hat = 0, se = 0, CI is (0,0). Since pi > 0, (0,0) does not contain pi.

# (d) Wilson's score interval for 1000 samples
wilson_contains <- sum(sapply(samples_1000, function(x) {
  ci <- prop.test(x, n, correct=FALSE)$conf.int
  ci[1] <= pi_true && ci[2] >= pi_true
}))
cat("1000 samples Wilson coverage percentage:", (wilson_contains / 1000) * 100, "%\n")

cat("Confidence Intervals Module R code executed successfully.\n")
