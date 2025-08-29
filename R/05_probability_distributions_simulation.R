# This project is to simulate probability distibutions
set.seed(1234)
poisson_dist <- rpois(1000, lambda = 5)
print(poisson_dist)
norm_dist <- rnorm(1000, mean = 5, sd = sqrt(5))
print(norm_dist)
bin_dist <- rbinom(1000, size = 10, prob = 0.5)
print(bin_dist)
chisq_dist <- rchisq(1000, df = 5)
print(chisq_dist)
gamma_dist <- rgamma(1000, shape = 5, rate = 1)
print(gamma_dist)
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
mean_bin <- mean(bin_dist)
sd_bin <- sd(bin_dist)
var_bin <- var(bin_dist)
cat("Simulated Binomial Distribution - Mean:", mean_bin, "SD:", sd_bin, "Var:", var_bin, "\n") # nolint
cat("Real Binomial Distribution - Mean:", 5, "SD:", sqrt(5 * 0.5 * 0.5), "Var:", 5 * 0.5 * 0.5, "\n") # nolint
mean_chisq <- mean(chisq_dist)
sd_chisq <- sd(chisq_dist)
var_chisq <- var(chisq_dist)
cat("Simulated Chi-Squared Distribution - Mean:", mean_chisq, "SD:", sd_chisq, "Var:", var_chisq, "\n") # nolint
cat("Real Chi-Squared Distribution - Mean:", 5, "SD:", sqrt(10), "Var:", 10, "\n") # nolint
mean_gamma <- mean(gamma_dist)
sd_gamma <- sd(gamma_dist)
var_gamma <- var(gamma_dist)
cat("Simulated Gamma Distribution - Mean:", mean_gamma, "SD:", sd_gamma, "Var:", var_gamma, "\n") # nolint
cat("Real Gamma Distribution - Mean:", 5, "SD:", sqrt(5), "Var:", 5, "\n") # nolint
# Visualizing the distributions
par(mfrow = c(2, 2))
hist(poisson_dist, main = "Simulated Poisson Distribution", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black") # nolint
hist(norm_dist, main = "Simulated Normal Distribution", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black") # nolint
hist(bin_dist, main = "Simulated Binomial Distribution", xlab = "Value", ylab = "Frequency", col = "lightpink", border = "black") # nolint
hist(chisq_dist, main = "Simulated Chi-Squared Distribution", xlab = "Value", ylab = "Frequency", col = "lightyellow", border = "black") # nolint
hist(gamma_dist, main = "Simulated Gamma Distribution", xlab = "Value", ylab = "Frequency", col = "lightgray", border = "black") # nolint
