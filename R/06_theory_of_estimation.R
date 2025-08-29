# this project is about the theory of estimation in statistics
print("Enter the number of data points needed")
u_dist_in <- as.integer(readLines(n = 1))
distr <- rnorm(u_dist_in)
mean_distr <- mean(distr)
std_dev_distr <- sd(distr)
print("Enter the number of data points to sample")
u_samp_in <- as.integer(readLines(n = 1))
if (u_samp_in > 0 && u_samp_in <= u_dist_in) {
    sample_distr <- sample(distr, u_samp_in) # nolint
    mean_sample_distr <- mean(sample_distr)
    std_dev_sample_distr <- sd(sample_distr)
} else {
    stop("Invalid sample size") # nolint
}
# visualization
plot(density(distr), main = "Density Plot of Distribution and Sample", col = "blue", lwd = 2) # nolint
if (exists("sample_distr")) {
    lines(density(sample_distr), col = "red", lwd = 2) # nolint
}
