# This project is to simulate probability distibutions
set.seed(1234)
poisson_dist <- rpois(1000, lambda = 5)
norm_dist <- rnorm(1000, mean = 5, sd = sqrt(5))
bin_dist <- rbinom(1000, size = 10, prob = 0.5)
chisq_dist <- rchisq(1000, df = 5)
gamma_dist <- rgamma(1000, shape = 5, rate = 1)
exp_dist <- rexp(1000, rate = 1)

# comparing theoretical and simulated values
compare_values <- function(simulated, mean_theo, var_theo, name) {
    mean <- mean(simulated) # nolint
    sd <- sd(simulated)
    var <- var(simulated)
    cat(sprintf(
        "Comparing %s Distribution - Mean: Simulated = %.2f, Theoretical = %.2f, Difference = %.2f\n", # nolint
        name, mean, mean_theo, mean - mean_theo
    )) # nolint
    cat(sprintf(
        "Comparing %s Distribution - SD: Simulated = %.2f, Theoretical = %.2f, Difference = %.2f\n", # nolint
        name, sd, sqrt(var_theo), sd - sqrt(var_theo)
    )) # nolint
    cat(sprintf(
        "Comparing %s Distribution - Var: Simulated = %.2f, Theoretical = %.2f, Difference = %.2f\n", # nolint
        name, var, var_theo, var - var_theo
    )) # nolint
}
compare_values(poisson_dist, 5, 5, "Poisson")
compare_values(norm_dist, 5, 5, "Normal")
compare_values(bin_dist, 5, 5 * 0.5 * 0.5, "Binomial")
compare_values(chisq_dist, 5, 10, "Chi-Squared")
compare_values(gamma_dist, 5, 5, "Gamma")
compare_values(exp_dist, 1, 1, "Exponential")

# Visualizing the distributions
par(mfrow = c(2, 2))
hist(poisson_dist, main = "Simulated Poisson Distribution", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black") # nolint
hist(norm_dist, main = "Simulated Normal Distribution", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black") # nolint
hist(bin_dist, main = "Simulated Binomial Distribution", xlab = "Value", ylab = "Frequency", col = "lightpink", border = "black") # nolint
hist(chisq_dist, main = "Simulated Chi-Squared Distribution", xlab = "Value", ylab = "Frequency", col = "lightyellow", border = "black") # nolint
hist(gamma_dist, main = "Simulated Gamma Distribution", xlab = "Value", ylab = "Frequency", col = "lightgray", border = "black") # nolint
hist(exp_dist, main = "Simulated Exponential Distribution", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black") # nolint

# demonstration of central limit theorem
CLT <- function(rfunc, params, mean_theo, var_theo, dist_name, n_samples, sample_size) { # nolint
    sample_means <- replicate(1000, mean(do.call(rfunc, c(list(sample_size), params)))) # nolint
    cat("Simulated CLT - Mean:", mean(sample_means), "SD:", sd(sample_means), "\n") # nolint
    cat("Real CLT - Mean:", mean_theo, "SD:", sqrt(var_theo / n_samples), "\n")
    hist(sample_means,
        probability = TRUE, # nolint
        main = paste("Simulated CLT -", dist_name), xlab = "Sample Mean", ylab = "Frequency", col = "lightblue", border = "black" # nolint
    ) # nolint
    curve(dnorm(x, mean = mean_theo, sd = sqrt(var_theo / sample_size)), add = TRUE, col = "#f26464", lwd = 2) # nolint
}
par(mfrow = c(2, 2))
CLT("rpois", list(lambda = 5), mean_theo = 5, var_theo = 5, dist_name = "Poisson", n_samples = 1000, sample_size = 30) # nolint
CLT("rnorm", list(mean = 5, sd = sqrt(5)), mean_theo = 5, var_theo = 5, dist_name = "Normal", n_samples = 1000, sample_size = 30) # nolint
CLT("rbinom", list(size = 10, prob = 0.5), mean_theo = 5, var_theo = 5 * 0.5 * 0.5, dist_name = "Binomial", n_samples = 1000, sample_size = 30) # nolint
CLT("rchisq", list(df = 5), mean_theo = 5, var_theo = 10, dist_name = "Chi-Squared", n_samples = 1000, sample_size = 30) # nolint
CLT("rgamma", list(shape = 5, rate = 1), mean_theo = 5, var_theo = 5, dist_name = "Gamma", n_samples = 1000, sample_size = 30) # nolint
CLT("rexp", list(rate = 1), mean_theo = 1, var_theo = 1, dist_name = "Exponential", n_samples = 1000, sample_size = 30) # nolint
# next update will include user input for lambda value, number of simulations, distribution type, sample size, etc. #nolint
# Convergence checks
# example of poisson distribution
lambda <- 5
dist_poisson <- rpois(1000, lambda)
check_convergence <- function(data, lambda) {
    n <- length(data) # nolint
    cum_mean <- cumsum(data) / (1:n)
    plot(1:n, cum_mean, type = "l", col = "#3737af", lwd = 2, ylim = c(min(cum_mean, lambda) - 1, max(cum_mean, lambda) + 1), xlab = "Number of Simulations", ylab = "Cumulative Mean", main = "Convergence of Sample Mean to Theoretical Mean") # nolint
    abline(h = lambda, col = "#1b76ee", lty = 2)
    legend("topright", legend = c("Cumulative Mean", "Theoretical Mean"), col = c("#4a4ae7", "#ca4747"), lty = c(1, 2), lwd = 2) # nolint
}
check_convergence(dist_poisson, lambda)
# goodness of fit test
# example of poisson distribution
pois_table <- table(dist_poisson)
pois_chisq <- chisq.test(pois_table, p = dpois(as.numeric(names), lambda = lambda), rescale.p = TRUE) # nolint
print(pois_chisq)

# LLN
LLN_demo <- function(rfunc, params, mean_theo, var_theo, dist_name, max_n) { # nolint
    sample_sizes <- seq(10, max_n, by = 10) # nolint
    sample_means <- sapply(sample_sizes, function(n) mean(do.call(rfunc, c(list(n), params)))) # nolint
    plot(sample_sizes, sample_means, type = "l", col = "#363686", lwd = 2, ylim = c(min(sample_means, mean_theo) - 1, max(sample_means, mean_theo) + 1), xlab = "Sample Size", ylab = "Sample Mean", main = paste("LLN Demonstration -", dist_name)) # nolint
    abline(h = mean_theo, col = "#76e60e", lty = 2)
    legend("topright", legend = c("Sample Mean", "Theoretical Mean"), col = c("#333391", "#b94444"), lty = c(1, 2), lwd = 2) # nolint
}
par(mfrow = c(2, 2))
LLN_demo("rpois", list(lambda = 5), mean_theo = 5, var_theo = 5, dist_name = "Poisson", max_n = 1000) # nolint
LLN_demo("rnorm", list(mean = 5, sd = sqrt(5)), mean_theo = 5, var_theo = 5, dist_name = "Normal", max_n = 1000) # nolint
LLN_demo("rbinom", list(size = 10, prob = 0.5), mean_theo = 5, var_theo = 5 * 0.5 * 0.5, dist_name = "Binomial", max_n = 1000) # nolint
LLN_demo("rchisq", list(df = 5), mean_theo = 5, var_theo = 10, dist_name = "Chi-Squared", max_n = 1000) # nolint
