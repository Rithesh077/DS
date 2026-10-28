
# sta401 - design of experiments
# lab assignment 6: latin square design
#
# introduction:
# a latin square design controls two sources of nuisance variation: rows and columns.
# each treatment occurs exactly once in every row and once in every column.
#
# data description:
# the data contain a 5 x 5 latin square with 25 yield observations.
# treatments a, b, c, d and e are the five varieties.
# row and column represent two directions of field variability.
#
# significance level: 0.05

alpha <- 0.05
k <- 5

# question 1: treatment effects after accounting for rows and columns
# part a: enter the data from the assignment table, row by row.

row <- factor(rep(1:5, each = 5))
column <- factor(rep(1:5, times = 5))

treatment <- factor(c(
  "a", "b", "c", "d", "e",
  "b", "c", "d", "e", "a",
  "c", "d", "e", "a", "b",
  "d", "e", "a", "b", "c",
  "e", "a", "b", "c", "d"
), levels = c("a", "b", "c", "d", "e"))

yield <- c(
  48.8, 51.4, 52.2, 53.6, 53.5,
  52.6, 53.5, 53.5, 54.1, 50.2,
  54.1, 49.8, 52.9, 49.4, 50.7,
  52.1, 53.4, 53.8, 54.3, 53.4,
  50.3, 48.0, 50.7, 52.3, 49.5
)

data_lsd <- data.frame(row, column, treatment, yield)

print("question 1: latin square data")
print(data_lsd)
print("there are 25 observations, five rows, five columns and five treatments.")
print("each treatment occurs once in each row and once in each column.")

# verify that the layout is a valid latin square.
row_check <- with(data_lsd, table(row, treatment))
column_check <- with(data_lsd, table(column, treatment))
print("treatment counts by row; every count should be 1")
print(row_check)
print("treatment counts by column; every count should be 1")
print(column_check)

if (all(row_check == 1) && all(column_check == 1)) {
  print("design check passed: this is a valid 5 x 5 latin square.")
} else {
  print("design check failed: inspect the treatment layout.")
}

# model: yield = overall mean + row effect + column effect + treatment effect + error.
# h0: all five treatment population means are equal.
# h1: at least one treatment population mean differs.

fit_lsd <- aov(yield ~ row + column + treatment, data = data_lsd)
anova_lsd <- summary(fit_lsd)[[1]]

print("question 1: complete latin square anova table")
print(anova_lsd)

treatment_p <- anova_lsd["treatment", "Pr(>F)"]
error_ms_lsd <- anova_lsd["Residuals", "Mean Sq"]
error_df_lsd <- anova_lsd["Residuals", "Df"]

print(paste("treatment f-statistic =", round(anova_lsd["treatment", "F value"], 5)))
print(paste("treatment p-value =", signif(treatment_p, 6)))
print(paste("error degrees of freedom =", error_df_lsd))

if (error_df_lsd == (k - 1) * (k - 2)) {
  print("error degrees of freedom check passed: (k - 1)(k - 2) = 12.")
} else {
  print("error degrees of freedom check failed; inspect the model and data.")
}

if (treatment_p < alpha) {
  print("decision: reject h0. there is evidence that at least one variety mean differs after accounting for rows and columns.")
} else {
  print("decision: fail to reject h0. there is insufficient evidence of a treatment effect at the 5% level.")
}

# part c: compare treatment means using tukey's honestly significant difference.
tukey_lsd <- TukeyHSD(fit_lsd, "treatment", conf.level = 1 - alpha)
print("tukey hsd comparisons for treatment")
print(tukey_lsd)

treatment_means <- tapply(data_lsd$yield, data_lsd$treatment, mean)
print("raw treatment means, for descriptive ranking")
print(treatment_means)
print("treatments ranked by raw mean yield, highest first")
print(sort(treatment_means, decreasing = TRUE))

# each treatment occurs five times. calculate the t-based critical difference.
replications <- k
sed_lsd <- sqrt(2 * error_ms_lsd / replications)
critical_difference <- qt(1 - alpha / 2, error_df_lsd) * sed_lsd

# calculate the tukey hsd cutoff as well; tukey is the multiple-comparison procedure.
tukey_critical_difference <- qtukey(1 - alpha, nmeans = k, df = error_df_lsd) *
  sqrt(error_ms_lsd / replications)

print(paste("standard error of difference =", round(sed_lsd, 5)))
print(paste("t-based critical difference at 5% =", round(critical_difference, 5)))
print(paste("tukey hsd critical difference at 5% =", round(tukey_critical_difference, 5)))
print("a tukey adjusted p-value below 0.05 indicates a significant pairwise difference.")
print("a tukey confidence interval that excludes zero also indicates a significant difference.")

# part d: check model assumptions.
residuals_lsd <- residuals(fit_lsd)
shapiro_lsd <- shapiro.test(residuals_lsd)
bartlett_lsd <- bartlett.test(yield ~ treatment, data = data_lsd)

print("shapiro-wilk test for normality of residuals")
print(shapiro_lsd)
if (shapiro_lsd$p.value < alpha) {
  print("normality check: reject the normality null hypothesis at the 5% level.")
} else {
  print("normality check: fail to reject the normality null hypothesis at the 5% level.")
}

print("bartlett test for equal variances across treatments")
print(bartlett_lsd)
if (bartlett_lsd$p.value < alpha) {
  print("variance check: reject the equal-variances null hypothesis at the 5% level.")
} else {
  print("variance check: fail to reject the equal-variances null hypothesis at the 5% level.")
}

par(mfrow = c(1, 2))
plot(fitted(fit_lsd), residuals_lsd,
     xlab = "fitted values", ylab = "residuals",
     main = "residuals versus fitted")
abline(h = 0)
qqnorm(residuals_lsd, main = "normal q-q plot")
qqline(residuals_lsd)
par(mfrow = c(1, 1))

print("diagnostic interpretation: look for residuals scattered around zero without a clear pattern.")
print("in the q-q plot, points roughly following the line are consistent with approximate normality.")
print("non-significant assumption tests do not prove that assumptions are true.")

# part e: compare with a completely randomized design that ignores rows and columns.
fit_crd_q1 <- aov(yield ~ treatment, data = data_lsd)
anova_crd_q1 <- summary(fit_crd_q1)[[1]]
crd_treatment_p <- anova_crd_q1["treatment", "Pr(>F)"]

print("crd anova table, ignoring row and column effects")
print(anova_crd_q1)
print(paste("crd treatment p-value =", signif(crd_treatment_p, 6)))

if (crd_treatment_p < alpha) {
  print("crd decision: reject the equal-treatment-means null hypothesis at the 5% level.")
} else {
  print("crd decision: fail to reject the equal-treatment-means null hypothesis at the 5% level.")
}

if ((treatment_p < alpha) == (crd_treatment_p < alpha)) {
  print("the lsd and crd reach the same reject/fail-to-reject decision for these data.")
} else {
  print("the lsd and crd reach different reject/fail-to-reject decisions for these data.")
}
print("ignoring row and column effects can put nuisance variation into the error term and change the treatment test.")

# question 2: efficiency of the latin square design
# part a: fit an rbd blocking on rows only, and a crd with no blocking.

fit_rbd <- aov(yield ~ row + treatment, data = data_lsd)
fit_crd <- aov(yield ~ treatment, data = data_lsd)
anova_rbd <- summary(fit_rbd)[[1]]
anova_crd <- summary(fit_crd)[[1]]

mse_lsd <- deviance(fit_lsd) / df.residual(fit_lsd)
mse_rbd <- deviance(fit_rbd) / df.residual(fit_rbd)
mse_crd <- deviance(fit_crd) / df.residual(fit_crd)

df_lsd <- df.residual(fit_lsd)
df_rbd <- df.residual(fit_rbd)
df_crd <- df.residual(fit_crd)

print("rbd anova table: blocking on rows only")
print(anova_rbd)
print("crd anova table: ignoring rows and columns")
print(anova_crd)

print(paste("lsd error mean square =", round(mse_lsd, 6)))
print(paste("lsd error degrees of freedom =", df_lsd))
print(paste("rbd error mean square =", round(mse_rbd, 6)))
print(paste("rbd error degrees of freedom =", df_rbd))
print(paste("crd error mean square =", round(mse_crd, 6)))
print(paste("crd error degrees of freedom =", df_crd))

# part b: relative efficiency formula specified in the assignment.
# re = (mse_alt / mse_lsd) *
#      ((df_lsd + 1)(df_alt + 3)) / ((df_lsd + 3)(df_alt + 1)).
re_lsd_vs_rbd <- (mse_rbd / mse_lsd) *
  ((df_lsd + 1) * (df_rbd + 3)) /
  ((df_lsd + 3) * (df_rbd + 1))

re_lsd_vs_crd <- (mse_crd / mse_lsd) *
  ((df_lsd + 1) * (df_crd + 3)) /
  ((df_lsd + 3) * (df_crd + 1))

print(paste("relative efficiency of lsd compared with rbd =", round(re_lsd_vs_rbd, 5)))
print(paste("relative efficiency of lsd compared with crd =", round(re_lsd_vs_crd, 5)))

if (re_lsd_vs_rbd > 1) {
  print("interpretation: lsd is more efficient than rbd under the assignment's adjusted formula.")
} else if (re_lsd_vs_rbd < 1) {
  print("interpretation: lsd is less efficient than rbd under the assignment's adjusted formula.")
} else {
  print("interpretation: lsd and rbd have equal efficiency under the assignment's adjusted formula.")
}

if (re_lsd_vs_crd > 1) {
  print("interpretation: lsd is more efficient than crd under the assignment's adjusted formula.")
} else if (re_lsd_vs_crd < 1) {
  print("interpretation: lsd is less efficient than crd under the assignment's adjusted formula.")
} else {
  print("interpretation: lsd and crd have equal efficiency under the assignment's adjusted formula.")
}

print("the degrees-of-freedom adjustment accounts for differences in residual degrees of freedom.")
print("using only the raw mse ratio would ignore those differences and could give a misleading comparison.")

# conclusion:
# the latin square model compares variety yields after accounting for row and column gradients.
# use the treatment p-value for the overall treatment conclusion, tukey results for differing pairs,
# and residual diagnostics to assess assumptions.
# compare adjusted relative efficiencies with rbd and crd before recommending a design.
