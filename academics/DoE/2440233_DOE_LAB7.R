# sta401 - design of experiments
# lab assignment 7: missing plot technique
#
# introduction:
# this program estimates one missing observation in a randomised block design
# and one missing observation in a latin square design.
# the estimates are verified using linear models, followed by anova and
# treatment significance tests at the 5% level.
#
# data description:
# question 1 contains four treatments and five blocks. treatment b in block 3 is missing.
# question 2 contains a 4 x 4 latin square. treatment d in row 2, column 3 is missing.
#
# significance level: 0.05

alpha <- 0.05

# question 1: randomised block design with one missing observation
#
# missing plot formula:
# x = (t * treatment_total + r * block_total - grand_total) /
#     ((t - 1) * (r - 1))
#
# all totals must be calculated using observed values only.

treatments_q1 <- c("a", "b", "c", "d")
blocks_q1 <- paste0("block_", 1:5)

yield_matrix_q1 <- matrix(
  c(
    40.0, 43.4, 41.2, 37.9,
    40.5, 42.8, 42.6, 41.6,
    38.4, NA,   41.1, 38.4,
    40.6, 42.4, 42.0, 40.3,
    37.9, 42.0, 38.7, 37.0
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(blocks_q1, treatments_q1)
)

print("question 1: randomised block design")
print("observed yield data")
print(yield_matrix_q1)
print("there are four treatments and five blocks.")
print("treatment b in block 3 is the missing observation.")

t_q1 <- length(treatments_q1)
r_q1 <- length(blocks_q1)

treatment_total_b <- sum(yield_matrix_q1[, "b"], na.rm = TRUE)
block_total_3 <- sum(yield_matrix_q1["block_3", ], na.rm = TRUE)
grand_total_q1 <- sum(yield_matrix_q1, na.rm = TRUE)

print("totals calculated from observed values only")
print(paste("treatment b total =", treatment_total_b))
print(paste("block 3 total =", block_total_3))
print(paste("observed grand total =", grand_total_q1))

missing_q1 <- (
  t_q1 * treatment_total_b +
    r_q1 * block_total_3 -
    grand_total_q1
) / ((t_q1 - 1) * (r_q1 - 1))

print(paste("estimated missing yield =", round(missing_q1, 4)))

# verify the estimate using the observed data and an additive model.

data_q1_observed <- data.frame(
  block = factor(rep(blocks_q1, each = t_q1), levels = blocks_q1),
  treatment = factor(rep(treatments_q1, times = r_q1), levels = treatments_q1),
  yield = as.vector(t(yield_matrix_q1))
)

data_q1_observed <- data_q1_observed[
  !is.na(data_q1_observed$yield), 
]

model_q1_observed <- lm(
  yield ~ block + treatment,
  data = data_q1_observed
)

missing_cell_q1 <- data.frame(
  block = factor("block_3", levels = blocks_q1),
  treatment = factor("b", levels = treatments_q1)
)

predicted_q1 <- as.numeric(
  predict(model_q1_observed, newdata = missing_cell_q1)
)

print(paste("model prediction for missing yield =", round(predicted_q1, 4)))

if (isTRUE(all.equal(missing_q1, predicted_q1, tolerance = 1e-6))) {
  print("verification passed: formula estimate agrees with model prediction.")
} else {
  print("verification warning: formula estimate and model prediction differ.")
}

# insert the estimate and carry out the rbd anova.
# one error degree of freedom is lost because the missing value was estimated.

yield_matrix_q1_completed <- yield_matrix_q1
yield_matrix_q1_completed["block_3", "b"] <- missing_q1

data_q1_completed <- data.frame(
  block = factor(rep(blocks_q1, each = t_q1), levels = blocks_q1),
  treatment = factor(rep(treatments_q1, times = r_q1), levels = treatments_q1),
  yield = as.vector(t(yield_matrix_q1_completed))
)

model_q1_completed <- aov(
  yield ~ block + treatment,
  data = data_q1_completed
)

anova_q1 <- summary(model_q1_completed)[[1]]

print("question 1: anova table")
print(anova_q1)

ss_treatment_q1 <- anova_q1["treatment", "Sum Sq"]
df_treatment_q1 <- t_q1 - 1
ss_error_q1 <- deviance(model_q1_completed)
df_error_q1 <- df.residual(model_q1_completed) - 1

ms_treatment_q1 <- ss_treatment_q1 / df_treatment_q1
ms_error_q1 <- ss_error_q1 / df_error_q1

f_treatment_q1 <- ms_treatment_q1 / ms_error_q1

p_treatment_q1 <- pf(
  f_treatment_q1,
  df_treatment_q1,
  df_error_q1,
  lower.tail = FALSE
)

print("question 1: adjusted anova results")
print(paste("treatment sum of squares =", round(ss_treatment_q1, 5)))
print(paste("treatment degrees of freedom =", df_treatment_q1))
print(paste("error sum of squares =", round(ss_error_q1, 5)))
print(paste("error degrees of freedom =", df_error_q1))
print(paste("treatment mean square =", round(ms_treatment_q1, 5)))
print(paste("error mean square =", round(ms_error_q1, 5)))
print(paste("treatment f statistic =", round(f_treatment_q1, 5)))
print(paste("treatment p value =", signif(p_treatment_q1, 6)))

print("the complete rbd error degrees of freedom are (4 - 1)(5 - 1) = 12.")
print("one degree of freedom is removed because the missing observation was estimated.")

if (p_treatment_q1 < alpha) {
  print("conclusion: reject the null hypothesis. treatment means differ significantly at the 5% level.")
} else {
  print("conclusion: fail to reject the null hypothesis. there is insufficient evidence of treatment differences at the 5% level.")
}

# question 2: latin square design with one missing observation
#
# missing plot formula for a latin square of order p:
# x = (p * (row_total + column_total + treatment_total) - 2 * grand_total) /
#     ((p - 1) * (p - 2))
#
# all totals must be calculated using observed values only.

p_q2 <- 4
rows_q2 <- paste0("row_", 1:4)
columns_q2 <- paste0("column_", 1:4)
treatments_q2 <- c("a", "b", "c", "d")

treatment_matrix_q2 <- matrix(
  c(
    "a", "b", "c", "d",
    "b", "c", "d", "a",
    "c", "d", "a", "b",
    "d", "a", "b", "c"
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(rows_q2, columns_q2)
)

yield_matrix_q2 <- matrix(
  c(
    42.8, 46.5, 45.4, 44.8,
    48.7, 46.1, NA,   45.7,
    45.4, 43.1, 43.6, 46.7,
    43.8, 44.1, 50.5, 46.1
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(rows_q2, columns_q2)
)

print("question 2: latin square design")
print("treatment layout")
print(treatment_matrix_q2)
print("observed yield data")
print(yield_matrix_q2)
print("there are four rows, four columns and four treatments.")
print("treatment d in row 2, column 3 is the missing observation.")

row_total_2 <- sum(yield_matrix_q2["row_2", ], na.rm = TRUE)
column_total_3 <- sum(yield_matrix_q2[, "column_3"], na.rm = TRUE)
treatment_total_d <- sum(
  yield_matrix_q2[treatment_matrix_q2 == "d"],
  na.rm = TRUE
)
grand_total_q2 <- sum(yield_matrix_q2, na.rm = TRUE)

print("totals calculated from observed values only")
print(paste("row 2 total =", row_total_2))
print(paste("column 3 total =", column_total_3))
print(paste("treatment d total =", treatment_total_d))
print(paste("observed grand total =", grand_total_q2))

missing_q2 <- (
  p_q2 * (row_total_2 + column_total_3 + treatment_total_d) -
    2 * grand_total_q2
) / ((p_q2 - 1) * (p_q2 - 2))

print(paste("estimated missing yield =", round(missing_q2, 4)))

# verify the estimate using an additive model with row, column and treatment effects.

data_q2_observed <- data.frame(
  row = factor(rep(rows_q2, each = p_q2), levels = rows_q2),
  column = factor(rep(columns_q2, times = p_q2), levels = columns_q2),
  treatment = factor(
    as.vector(t(treatment_matrix_q2)),
    levels = treatments_q2
  ),
  yield = as.vector(t(yield_matrix_q2))
)

data_q2_observed <- data_q2_observed[
  !is.na(data_q2_observed$yield),
]

model_q2_observed <- lm(
  yield ~ row + column + treatment,
  data = data_q2_observed
)

missing_cell_q2 <- data.frame(
  row = factor("row_2", levels = rows_q2),
  column = factor("column_3", levels = columns_q2),
  treatment = factor("d", levels = treatments_q2)
)

predicted_q2 <- as.numeric(
  predict(model_q2_observed, newdata = missing_cell_q2)
)

print(paste("model prediction for missing yield =", round(predicted_q2, 4)))

if (isTRUE(all.equal(missing_q2, predicted_q2, tolerance = 1e-6))) {
  print("verification passed: formula estimate agrees with model prediction.")
} else {
  print("verification warning: formula estimate and model prediction differ.")
}

# insert the estimate and carry out the latin square anova.
# one error degree of freedom is lost because the missing value was estimated.

yield_matrix_q2_completed <- yield_matrix_q2
yield_matrix_q2_completed["row_2", "column_3"] <- missing_q2

data_q2_completed <- data.frame(
  row = factor(rep(rows_q2, each = p_q2), levels = rows_q2),
  column = factor(rep(columns_q2, times = p_q2), levels = columns_q2),
  treatment = factor(
    as.vector(t(treatment_matrix_q2)),
    levels = treatments_q2
  ),
  yield = as.vector(t(yield_matrix_q2_completed))
)

model_q2_completed <- aov(
  yield ~ row + column + treatment,
  data = data_q2_completed
)

anova_q2 <- summary(model_q2_completed)[[1]]

print("question 2: anova table")
print(anova_q2)

ss_treatment_q2 <- anova_q2["treatment", "Sum Sq"]
df_treatment_q2 <- p_q2 - 1
ss_error_q2 <- deviance(model_q2_completed)
df_error_q2 <- df.residual(model_q2_completed) - 1

ms_treatment_q2 <- ss_treatment_q2 / df_treatment_q2
ms_error_q2 <- ss_error_q2 / df_error_q2

f_treatment_q2 <- ms_treatment_q2 / ms_error_q2

p_treatment_q2 <- pf(
  f_treatment_q2,
  df_treatment_q2,
  df_error_q2,
  lower.tail = FALSE
)

print("question 2: adjusted anova results")
print(paste("treatment sum of squares =", round(ss_treatment_q2, 5)))
print(paste("treatment degrees of freedom =", df_treatment_q2))
print(paste("error sum of squares =", round(ss_error_q2, 5)))
print(paste("error degrees of freedom =", df_error_q2))
print(paste("treatment mean square =", round(ms_treatment_q2, 5)))
print(paste("error mean square =", round(ms_error_q2, 5)))
print(paste("treatment f statistic =", round(f_treatment_q2, 5)))
print(paste("treatment p value =", signif(p_treatment_q2, 6)))

complete_lsd_error_df_q2 <- (p_q2 - 1) * (p_q2 - 2)
lost_df_q2 <- complete_lsd_error_df_q2 - df_error_q2

print(paste("complete latin square error degrees of freedom =", complete_lsd_error_df_q2))
print(paste("error degrees of freedom after estimating the missing value =", df_error_q2))
print(paste("error degrees of freedom lost =", lost_df_q2))

if (lost_df_q2 == 1) {
  print("one error degree of freedom was lost because one observation was estimated.")
} else {
  print("check the error degrees of freedom calculation.")
}

if (p_treatment_q2 < alpha) {
  print("conclusion: reject the null hypothesis. at least one treatment mean differs significantly at the 5% level.")
} else {
  print("conclusion: fail to reject the null hypothesis. there is insufficient evidence of treatment differences at the 5% level.")
}

# overall conclusion:
# the missing plot formulas estimate the lost observations from observed totals.
# linear model predictions are used to verify the estimates.
# one error degree of freedom is removed in each analysis because one observation was estimated.
# treatment conclusions are based on the adjusted f tests and p values.