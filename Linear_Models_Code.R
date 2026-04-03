# ==========================================
# MODULE 4: LINEAR MODELS
# ==========================================

# ----------------------------------------------------
# Problem 1 (Exercise 6.1)
# ----------------------------------------------------
races_data <- read.table("ScotsRaces.dat", header=TRUE)

# (a) Scatterplot and prediction equation
pdf("Linear_prob1_scatterplot.pdf", width=6, height=5)
plot(races_data$timeW, races_data$timeM, main="Scottish Hill Races: Men's vs Women's Record Times",
     xlab="Women's Record Time (min)", ylab="Men's Record Time (min)", pch=19, col="darkblue")

model_1a <- lm(timeM ~ timeW, data=races_data)
abline(model_1a, col="red", lwd=2)
dev.off()

cat("\n--- Problem 1 (Exercise 6.1) ---\n")
cat("Prediction equation: timeM_hat =", coef(model_1a)[1], "+", coef(model_1a)[2], "* timeW\n")
# Predict for Highland Fling, timeW = 490.05
timeM_pred <- predict(model_1a, newdata=data.frame(timeW=490.05))
cat("Predicted men's record time for Highland Fling (timeW = 490.05):", timeM_pred, "\n")

# (b) Find and interpret correlation
correlation_1b <- cor(races_data$timeW, races_data$timeM)
cat("Correlation between timeW and timeM:", correlation_1b, "\n")

# (c) Model with natural constraint: E(Y) = beta * x
model_1c <- lm(timeM ~ -1 + timeW, data=races_data)
cat("Model through origin slope:", coef(model_1c)[1], "\n")


# ----------------------------------------------------
# Problem 2 (Exercise 6.3)
# ----------------------------------------------------
firearms_data <- read.table("Firearms2.dat", header=TRUE)

model_2 <- lm(Rate ~ Ownership, data=firearms_data)

pdf("Linear_prob2_plots.pdf", width=10, height=5)
par(mfrow=c(1,2))

# Scatterplot
plot(firearms_data$Ownership, firearms_data$Rate, main="Firearm Death Rate vs Ownership",
     xlab="Percentage Owning a Gun", ylab="Firearm Death Rate", pch=19, col="purple")
abline(model_2, col="orange", lwd=2)

# Cook's distance
cooksD <- cooks.distance(model_2)
plot(cooksD, type="h", main="Cook's Distance", ylab="Cook's D", xlab="Observation Index")
dev.off()

influential_idx <- which.max(cooksD)
influential_state <- firearms_data$State[influential_idx]
cat("\n--- Problem 2 (Exercise 6.3) ---\n")
cat("Potentially influential observation (max Cook's Distance):", 
    as.character(influential_state), "(Index:", influential_idx, ")\n")

cor_before <- cor(firearms_data$Ownership, firearms_data$Rate)
cor_after <- cor(firearms_data$Ownership[-influential_idx], firearms_data$Rate[-influential_idx])
cat("Correlation BEFORE removing influential observation:", cor_before, "\n")
cat("Correlation AFTER removing influential observation:", cor_after, "\n")


# ----------------------------------------------------
# Problem 3 (Exercise 6.5)
# ----------------------------------------------------
covid_data <- read.table("Covid19.dat", header=TRUE)

pdf("Linear_prob3_scatterplots.pdf", width=10, height=5)
par(mfrow=c(1,2))
# (a) Two scatterplots
plot(covid_data$day, covid_data$cases, main="Covid-19 Cases vs Time",
     xlab="Days", ylab="Cases", pch=19, col="brown")
plot(covid_data$day, log(covid_data$cases), main="Log(Covid-19 Cases) vs Time",
     xlab="Days", ylab="Log(Cases)", pch=19, col="darkgreen")
dev.off()

cat("\n--- Problem 3 (Exercise 6.5) ---\n")
# (b) Correlation
cor_cases <- cor(covid_data$day, covid_data$cases)
cor_log_cases <- cor(covid_data$day, log(covid_data$cases))
cat("Correlation between time and cases:", cor_cases, "\n")
cat("Correlation between time and log(cases):", cor_log_cases, "\n")

# (c) Fit linear model for log-transformed counts
model_3c <- lm(log(cases) ~ day, data=covid_data)
cat("Prediction equation: log(cases_hat) =", coef(model_3c)[1], "+", coef(model_3c)[2], "* day\n")
cat("exp(beta_1) = exp(", coef(model_3c)[2], ") =", exp(coef(model_3c)[2]), "\n")


# ----------------------------------------------------
# Problem 4 (Exercise 6.24)
# ----------------------------------------------------
x_sim <- c(1, 2, 3, 4, 5)
y_sim <- c(1.1, 1.9, 3.2, 4.0, 5.1)

cor_5pts <- cor(x_sim, y_sim)

# Add outlier
x_sim_new <- c(x_sim, 15)
y_sim_new <- c(y_sim, -10)
cor_6pts <- cor(x_sim_new, y_sim_new)

pdf("Linear_prob4_outlier.pdf", width=6, height=5)
plot(x_sim_new, y_sim_new, main="Effect of a Single Outlier",
     xlab="X", ylab="Y", pch=19, col=c(rep("blue", 5), "red"))
legend("topright", legend=c("Original 5 pts", "Outlier"), col=c("blue", "red"), pch=19)
dev.off()

cat("\n--- Problem 4 (Exercise 6.24) ---\n")
cat("Correlation with 5 points (close to +1):", cor_5pts, "\n")
cat("Correlation with 6th outlier point added:", cor_6pts, "\n")


# ----------------------------------------------------
# Problem 5 (Exercise 6.25)
# ----------------------------------------------------
# Simulated Guess the Correlation Game data (10 rounds)
set.seed(42)
actual_cor <- runif(10, -1, 1)
# Make guesses reasonably close to actual
guess_cor <- actual_cor + rnorm(10, mean=0, sd=0.1)
# Cap guesses between -1 and 1
guess_cor <- pmin(pmax(guess_cor, -1), 1)

guess_game <- data.frame(Actual=actual_cor, Guess=guess_cor)
cor_game <- cor(guess_game$Actual, guess_game$Guess)

cat("\n--- Problem 5 (Exercise 6.25) ---\n")
print(guess_game)
cat("Correlation between Guesses and Actuals:", cor_game, "\n")

cat("\nLinear Models Module R code executed successfully.\n")
