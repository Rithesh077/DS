# This project is to simulate probability distibutions
set.seed(1234)
poisson_dist <- rpois(1000, lambda = 5)
print(poisson_dist)
norm_dist <- rnorm(1000, mean = 5, sd = sqrt(5))
print(norm_dist)
# calculating mean and standard deviation and checking how close are they to the real values # nolint
mean_poisson <- mean(poisson_dist)
sd_poisson <- sd(poisson_dist)
var_poisson <- var(poisson_dist)
cat("Simulated Poisson Distribution - Mean:", mean_poisson, "SD:", sd_poisson, "Var:", var_poisson, "\n") # nolint
cat("Real Poisson Distribution - Mean:", 5, "SD:", sqrt(5), "Var:", 5, "\n") # nolint
mean_norm <- mean(norm_dist)
sd_norm <- sd(norm_dist)
var_norm <- var(norm_dist)
cat("Simulated Normal Distribution - Mean:", mean_norm, "SD:", sd_norm, "Var:", var_norm, "\n") # nolint
cat("Real Normal Distribution - Mean:", 5, "SD:", sqrt(5), "Var:", 5, "\n") # nolint
# Visualizing the distributions
par(mfrow = c(2, 2))
hist(poisson_dist, main = "Simulated Poisson Distribution", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black") # nolint
hist(norm_dist, main = "Simulated Normal Distribution", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black") # nolint
boxplot(poisson_dist, main = "Boxplot of Poisson Distribution", ylab = "Value", col = "lightblue") # nolint
boxplot(norm_dist, main = "Boxplot of Normal Distribution", ylab = "Value", col = "lightgreen") # nolint
