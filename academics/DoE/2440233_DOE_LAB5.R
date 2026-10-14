# 2440233 doe lab 5
# design of experiments
# aim: analyse randomized block designs, handle a missing observation, test assumptions,
# compare treatments, and measure the relative efficiency of blocking.
# significance level: 0.05
# all calculations use base r; no additional package is required.

alpha <- 0.05

# question 1: fertilizers and wheat yield
# introduction: a randomized block design compares treatments while controlling block variation.
# data description: four fertilizers were tested in five blocks. yield is measured in quintals per hectare.
# conclusion: fertilizer significance is assessed after accounting for blocks; mean comparisons and the
# critical difference identify which observed fertilizer means are separated.

set.seed(123)
fertilizer_names <- c("f1", "f2", "f3", "f4")
block_names <- paste0("block_", 1:5)

yield_matrix <- rbind(
  f1 = c(24, 29, 27, 32, 26),
  f2 = c(31, 35, 29, 30, 33),
  f3 = c(28, 30, 26, 34, 31),
  f4 = c(22, 25, 24, 28, 23)
)
colnames(yield_matrix) <- block_names

data_q1 <- data.frame(
  fertilizer = factor(rep(fertilizer_names, times = 5), levels = fertilizer_names),
  block = factor(rep(block_names, each = 4), levels = block_names),
  yield = as.vector(yield_matrix)
)

print("question 1: observed data")
print(data_q1)
print("data contain 20 plots, with one observation for each fertilizer in each block.")

# part a: generate one randomized fertilizer layout within every block.
random_layout <- do.call(rbind, lapply(block_names, function(b) {
  data.frame(block = b, plot = 1:4, fertilizer = sample(fertilizer_names))
}))
row.names(random_layout) <- NULL
print("part a: randomized layout (this is a generated allocation, separate from the supplied yield table)")
print(random_layout)

# part b: two-way additive anova is the standard rbd analysis.
fit_q1 <- aov(yield ~ block + fertilizer, data = data_q1)
table_q1 <- summary(fit_q1)[[1]]
print("part b: rbd analysis of variance")
print(table_q1)

p_block <- table_q1["block", "Pr(>F)"]
p_fertilizer <- table_q1["fertilizer", "Pr(>F)"]

print(paste("block p-value =", signif(p_block, 5)))
if (p_block < alpha) {
  print("blocks differ significantly at the 5% level.")
} else {
  print("there is insufficient evidence that block means differ at the 5% level.")
}
print(paste("fertilizer p-value =", signif(p_fertilizer, 5)))
if (p_fertilizer < alpha) {
  print("fertilizer means are not all equal at the 5% level.")
} else {
  print("there is insufficient evidence of a fertilizer effect at the 5% level.")
}

# part c: sed is the standard error of a difference between two treatment means.
means_q1 <- aggregate(yield ~ fertilizer, data_q1, mean)
error_ms <- table_q1["Residuals", "Mean Sq"]
error_df <- table_q1["Residuals", "Df"]
sed <- sqrt(2 * error_ms / length(block_names))
cd <- qt(1 - alpha / 2, error_df) * sed

print("fertilizer means")
print(means_q1)
print(paste("standard error of difference =", round(sed, 4)))
print(paste("critical difference at 5% =", round(cd, 4)))

pairs_q1 <- combn(as.character(means_q1$fertilizer), 2, simplify = FALSE)
for (one_pair in pairs_q1) {
  m1 <- means_q1$yield[means_q1$fertilizer == one_pair[1]]
  m2 <- means_q1$yield[means_q1$fertilizer == one_pair[2]]
  gap <- abs(m1 - m2)
  decision <- if (gap > cd) "significant" else "not significant"
  print(paste(one_pair[1], "versus", one_pair[2], ":", decision, "difference =", round(gap, 3)))
}
ranking_q1 <- means_q1[order(-means_q1$yield), ]
ranking_q1$rank <- seq_len(nrow(ranking_q1))
print("ranking by observed mean yield, highest first")
print(ranking_q1)

# part d: interaction plot is descriptive because the fitted rbd model has no treatment-by-block term.
interaction.plot(
  x.factor = data_q1$block,
  trace.factor = data_q1$fertilizer,
  response = data_q1$yield,
  xlab = "block",
  ylab = "yield (quintals per hectare)",
  trace.label = "fertilizer",
  main = "fertilizer yield across blocks"
)
print("plot interpretation: roughly parallel profiles indicate similar treatment patterns across blocks.")
print("crossing or strongly non-parallel profiles indicate that fertilizer differences vary across blocks.")
# conclusion: use the anova p-values for significance, the cd comparisons for pairwise differences,
# and the plotted profiles for a visual description of block-wise patterns.


# question 2: groundnut varieties with a missing observation
# introduction: the yield for v3 in block 2 is missing. estimate it using the standard rbd formula.
# data description: five varieties were observed in four blocks, giving 19 observed yields.
# conclusion: the incomplete-data linear model adjusts for the unbalanced design and gives the treatment
# sum of squares after accounting for blocks. estimating one value reduces the residual degrees of freedom.

varieties <- c("v1", "v2", "v3", "v4", "v5")
blocks <- paste0("block_", 1:4)
groundnut <- rbind(
  v1 = c(16, 20, 18, 23),
  v2 = c(22, 26, 21, 28),
  v3 = c(20, NA, 23, 25),
  v4 = c(15, 19, 13, 21),
  v5 = c(25, 28, 22, 29)
)
colnames(groundnut) <- blocks

data_q2 <- data.frame(
  variety = factor(rep(varieties, each = 4), levels = varieties),
  block = factor(rep(blocks, times = 5), levels = blocks),
  pod_yield = as.vector(t(groundnut))
)
print("question 2: observed data, with na marking the missing cell")
print(data_q2)

# part a: for t treatments and b blocks, estimate x using (t * treatment total + b * block total - grand total) / ((t - 1) * (b - 1)).
missing_index <- which(is.na(data_q2$pod_yield))
missing_variety <- as.character(data_q2$variety[missing_index])
missing_block <- as.character(data_q2$block[missing_index])
t <- nlevels(data_q2$variety)
b <- nlevels(data_q2$block)
treatment_total <- sum(data_q2$pod_yield[data_q2$variety == missing_variety], na.rm = TRUE)
block_total <- sum(data_q2$pod_yield[data_q2$block == missing_block], na.rm = TRUE)
grand_total <- sum(data_q2$pod_yield, na.rm = TRUE)
missing_value <- (t * treatment_total + b * block_total - grand_total) / ((t - 1) * (b - 1))
print(paste("estimated missing yield for v3 in block 2 =", round(missing_value, 4)))

filled_q2 <- data_q2
filled_q2$estimated_yield <- filled_q2$pod_yield
filled_q2$estimated_yield[missing_index] <- missing_value

# part b: display the classical filled-table sums of squares. use 11 residual df after estimating one cell.
filled_total <- sum(filled_q2$estimated_yield)
cf <- filled_total^2 / (t * b)
ss_total_filled <- sum(filled_q2$estimated_yield^2) - cf
treatment_totals <- tapply(filled_q2$estimated_yield, filled_q2$variety, sum)
block_totals <- tapply(filled_q2$estimated_yield, filled_q2$block, sum)
ss_treatment_filled <- sum(treatment_totals^2) / b - cf
ss_block_filled <- sum(block_totals^2) / t - cf
ss_error_filled <- ss_total_filled - ss_treatment_filled - ss_block_filled
df_error_adjusted <- (t - 1) * (b - 1) - 1

print("classical sums of squares after inserting the estimate")
print(paste("total ss =", round(ss_total_filled, 4)))
print(paste("treatment ss before incomplete-model adjustment =", round(ss_treatment_filled, 4)))
print(paste("block ss =", round(ss_block_filled, 4)))
print(paste("error ss from the filled table =", round(ss_error_filled, 4)))
print(paste("adjusted error degrees of freedom =", df_error_adjusted))
print("the filled-table treatment ss is shown as an intermediate calculation; use the incomplete-data model below for the adjusted treatment test.")

# part c: compare nested linear models on the 19 actually observed values.
observed_q2 <- data_q2[!is.na(data_q2$pod_yield), ]
model_blocks <- lm(pod_yield ~ block, data = observed_q2)
model_blocks_varieties <- lm(pod_yield ~ block + variety, data = observed_q2)
anova_models <- anova(model_blocks, model_blocks_varieties)
adjusted_treatment_ss <- deviance(model_blocks) - deviance(model_blocks_varieties)
error_ss_model <- deviance(model_blocks_varieties)
error_df_model <- df.residual(model_blocks_varieties)
error_ms_model <- error_ss_model / error_df_model
print("nested-model comparison: adding variety after block")
print(anova_models)
print(paste("adjusted treatment ss from the incomplete-data model =", round(adjusted_treatment_ss, 4)))
print(paste("incomplete-model error ss =", round(error_ss_model, 4)))
print(paste("incomplete-model error df =", error_df_model))
print(paste("incomplete-model error mean square =", round(error_ms_model, 4)))

# part d: compare estimated variety means using the model-based error mean square and residual df.
variety_means <- aggregate(estimated_yield ~ variety, filled_q2, mean)
sed_q2 <- sqrt(2 * error_ms_model / b)
cd_q2 <- qt(1 - alpha / 2, error_df_model) * sed_q2
print("variety means after replacing the missing value")
print(variety_means)
print(paste("standard error of difference =", round(sed_q2, 4)))
print(paste("critical difference at 5% =", round(cd_q2, 4)))

variety_pairs <- combn(as.character(variety_means$variety), 2, simplify = FALSE)
for (one_pair in variety_pairs) {
  m1 <- variety_means$estimated_yield[variety_means$variety == one_pair[1]]
  m2 <- variety_means$estimated_yield[variety_means$variety == one_pair[2]]
  gap <- abs(m1 - m2)
  decision <- if (gap > cd_q2) "significant" else "not significant"
  print(paste(one_pair[1], "versus", one_pair[2], ":", decision, "difference =", round(gap, 3)))
}
# conclusion: state the estimated cell value, adjusted model test, degrees of freedom, and cd comparisons.
# note that the model-based adjusted treatment ss and filled-table raw ss are different quantities.


# question 3: diets and weight gain in pigs
# introduction: litter is a block because pigs from one litter may be more alike than pigs from different litters.
# data description: five diets were assigned within each of four litters; weight gain is measured in kg.
# conclusion: compare rbd and crd error mean squares, test model assumptions, and use tukey comparisons
# to identify which diet pairs differ.

pig_gain <- rbind(
  d1 = c(12.3, 14.5, 11.0, 15.1),
  d2 = c(15.4, 17.1, 14.8, 17.5),
  d3 = c(13.9, 14.6, 12.4, 16.0),
  d4 = c(16.0, 18.5, 15.5, 18.1),
  d5 = c(12.9, 13.1, 11.3, 14.4)
)
colnames(pig_gain) <- paste0("litter_", 1:4)
data_q3 <- data.frame(
  diet = factor(rep(rownames(pig_gain), each = 4)),
  litter = factor(rep(colnames(pig_gain), times = nrow(pig_gain))),
  gain = as.vector(t(pig_gain))
)
print("question 3: pig weight-gain data")
print(data_q3)

# part a: randomized block analysis.
rbd_q3 <- aov(gain ~ litter + diet, data = data_q3)
anova_rbd <- summary(rbd_q3)[[1]]
print("rbd analysis of variance")
print(anova_rbd)
p_diet <- anova_rbd["diet", "Pr(>F)"]
p_litter <- anova_rbd["litter", "Pr(>F)"]
print(paste("diet p-value =", signif(p_diet, 5)))
print(if (p_diet < alpha) "diet effect is significant at 5%." else "diet effect is not significant at 5%.")
print(paste("litter p-value =", signif(p_litter, 5)))
print(if (p_litter < alpha) "litter effect is significant at 5%." else "litter effect is not significant at 5%.")

# part b: residual assumptions.
shapiro_result <- shapiro.test(residuals(rbd_q3))
bartlett_result <- bartlett.test(gain ~ diet, data = data_q3)
print("shapiro-wilk test of rbd residuals")
print(shapiro_result)
print(if (shapiro_result$p.value < alpha) "normality is rejected at 5%." else "normality is not rejected at 5%.")
print("bartlett test of variance equality across diets")
print(bartlett_result)
print(if (bartlett_result$p.value < alpha) "equal variances are rejected at 5%." else "equal variances are not rejected at 5%.")

par(mfrow = c(2, 2))
plot(rbd_q3)
par(mfrow = c(1, 1))
print("diagnostic plots: look for random scatter around zero, a roughly straight q-q plot, and a similar residual spread.")
print("a non-significant shapiro or bartlett test does not prove an assumption; interpret tests together with plots.")

# part c: tukey pairwise comparisons and grouping information.
tukey_result <- TukeyHSD(rbd_q3, "diet", conf.level = 0.95)
print("tukey hsd comparisons among diets")
print(tukey_result)
print("for each pair, an adjusted p-value below 0.05 indicates a significant difference.")
print("pairs with adjusted p-values at least 0.05 are not significantly different and may be reported in the same comparison group.")

# part d: completely randomized design ignores litter.
crd_q3 <- aov(gain ~ diet, data = data_q3)
anova_crd <- summary(crd_q3)[[1]]
print("crd analysis of variance, ignoring litter")
print(anova_crd)

mse_rbd <- anova_rbd["Residuals", "Mean Sq"]
mse_crd <- anova_crd["Residuals", "Mean Sq"]
f_rbd <- anova_rbd["diet", "F value"]
f_crd <- anova_crd["diet", "F value"]
relative_efficiency <- 100 * mse_crd / mse_rbd

print(paste("rbd error mean square =", round(mse_rbd, 6)))
print(paste("crd error mean square =", round(mse_crd, 6)))
print(paste("rbd diet f-value =", round(f_rbd, 6)))
print(paste("crd diet f-value =", round(f_crd, 6)))
print(paste("relative efficiency of rbd compared with crd (%) =", round(relative_efficiency, 2)))
if (mse_rbd < mse_crd) {
  print("the rbd has lower residual variation and is more precise by the error-mean-square criterion.")
} else if (mse_rbd > mse_crd) {
  print("the rbd has higher residual variation and is less precise by the error-mean-square criterion.")
} else {
  print("the rbd and crd have equal error mean squares.")
}
# conclusion: report significance, assumption checks, tukey pairwise results, and relative efficiency.
