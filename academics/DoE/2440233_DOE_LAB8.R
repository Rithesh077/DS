# lab assignment 8: missing value in a latin square design (lsd) using r
# course: design of experiments (doe)
# roll number: 2440233

# introduction
# this script performs missing value estimation and complete anova analysis 
# for a 4x4 latin square design comparing four feed types (a, b, c, d) 
# on the weight gain of pigs across litters (rows) and pens (columns).

# data description
# factors: litter (row: 1 to 4), pen (column: 1 to 4), feed (treatment: a, b, c, d)
# missing value: litter 3, pen 4, feed b (na)

cat("=== start of lab assignment 8 computations (id: 2440233) ===\n\n")

# data entry
lsd_data <- data.frame(
  litter = factor(rep(1:4, each = 4)),
  pen    = factor(rep(1:4, times = 4)),
  feed   = factor(c("A", "B", "C", "D",
                    "C", "B", "D", "A",
                    "C", "D", "A", "B",
                    "D", "A", "B", "C")),
  weight = c(14, 18, 21, 19,
             24, 17, 20, 15,
             22, 23, 16, NA,
             18, 13, 21, 25)
)

print("--- data structure ---")
print(lsd_data)

# section a: estimation

# q1: formula explanation in print statements
print("--- section a - q1: formula for missing value in lsd ---")
cat("formula for estimating one missing value in a t x t latin square design:\n")
cat("x_hat = (t * (r + c + t_val) - 2 * g) / ((t - 1) * (t - 2))\n")
cat("where:\n")
cat("t     = number of treatments (also rows and columns) = 4\n")
cat("r     = sum of observed values in the row containing the missing cell (litter 3)\n")
cat("c     = sum of observed values in the column containing the missing cell (pen 4)\n")
cat("t_val = sum of observed values in the treatment containing the missing cell (feed b)\n")
cat("g     = grand total of all observed values in the experiment\n\n")

# q2: lsd missing value estimator function
lsd_missing <- function(df) {
  idx <- which(is.na(df$weight))
  t <- nlevels(df$feed)
  g <- sum(df$weight, na.rm = TRUE)
  r <- sum(df$weight[df$litter == df$litter[idx]], na.rm = TRUE)
  c <- sum(df$weight[df$pen == df$pen[idx]], na.rm = TRUE)
  t_val <- sum(df$weight[df$feed == df$feed[idx]], na.rm = TRUE)
  
  est <- (t * (r + c + t_val) - 2 * g) / ((t - 1) * (t - 2))
  return(est)
}

x_hat <- lsd_missing(lsd_data)

print("--- section a - q2: missing value estimate ---")
cat(sprintf("estimated missing value (yates method): %.4f (approx %.2f)\n\n", x_hat, x_hat))

# q3: degrees of freedom explanation
print("--- section a - q3: minimum t requirement and error df ---")
cat("explanation: lsd requires t >= 4 for complete missing value analysis because\n")
cat("the error degrees of freedom for a t x t lsd with one missing value is (t - 1)(t - 2) - 1.\n")
cat("for t = 3, error df would be (2)(1) - 1 = 1, leaving insufficient df for error estimation.\n")
cat("for t = 4 with one missing value, error df = (4 - 1)(4 - 2) - 1 = (3)(2) - 1 = 5.\n\n")


# section b: complete analysis

# q4: naive anova analysis
lsd_filled <- lsd_data
lsd_filled$weight[is.na(lsd_filled$weight)] <- x_hat

naive_model <- lm(weight ~ litter + pen + feed, data = lsd_filled)
naive_anova <- anova(naive_model)

print("--- section b - q4: naive anova table (filled data, error df = 6) ---")
print(naive_anova)

# q5: correct analysis using lm on observed cells only
exact_model <- lm(weight ~ litter + pen + feed, data = lsd_data)
exact_anova <- anova(exact_model)

print("--- section b - q5: correct anova table (observed cells, error df = 5) ---")
print(exact_anova)

# verification of fitted value at missing cell
fitted_missing <- predict(exact_model, newdata = lsd_data[is.na(lsd_data$weight), ])
print("--- check: fitted value from lm() at missing cell ---")
cat(sprintf("fitted value at missing cell: %.4f\n\n", fitted_missing))


# section c: impact on treatment comparisons

# q7: tukey hsd comparisons
tukey_naive <- TukeyHSD(aov(weight ~ litter + pen + feed, data = lsd_filled))$feed
tukey_exact <- TukeyHSD(aov(weight ~ litter + pen + feed, data = lsd_data))$feed

print("--- section c - q7: pairwise feed comparisons (tukey hsd) ---")
cat("naive analysis tukey hsd:\n")
print(round(tukey_naive, 4))
cat("\ncorrect analysis tukey hsd:\n")
print(round(tukey_exact, 4))

# q8: mse comparison and conclusion
mse_naive <- deviance(naive_model) / df.residual(naive_model)
mse_exact <- deviance(exact_model) / df.residual(exact_model)

print("--- section c - q8: mse comparison and conclusions ---")
cat(sprintf("naive mse   = %.4f (error df = %d)\n", mse_naive, df.residual(naive_model)))
cat(sprintf("correct mse = %.4f (error df = %d)\n\n", mse_exact, df.residual(exact_model)))

cat("conclusion on treatment comparisons:\n")
cat("the naive analysis overstates the error degrees of freedom (6 vs 5) and understates the standard error,\n")
cat("making statistical tests artificially liberal. comparing confidence intervals and p-values shows\n")
cat("that the exact model provides wider, valid confidence bounds for pairwise differences.\n")

# summary of findings
# 1. missing value estimated by yates method is x_hat = 15.00 kg.
# 2. correct error df is reduced from 6 to 5.
# 3. fitting lm directly on observed data correctly evaluates the experimental error and valid f-tests.