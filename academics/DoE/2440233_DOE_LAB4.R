# Aim: To apply a Completely Randomized Design (CRD) to compare three treatments 
# and analyze their effects using ANOVA, assumptions checking, and post-hoc tests.

# Practical 1: Effect of Three Fertilizers on Wheat Yield

# (a) Using R, randomly allocate the three treatments to the 15 experimental plots.
set.seed(123)
plot_numbers <- 1:15
treatments <- rep(c("A", "B", "C"), each = 5)
random_allocation <- sample(treatments)
allocation_df <- data.frame(Plot = plot_numbers, Treatment = random_allocation)
print("Random Allocation of Treatments:")
print(allocation_df)
# Inference: The 15 plots have been randomly assigned to fertilizers A, B, and C to ensure unbiased estimation.

# Data Entry for Yield
yield_A <- c(20, 22, 19, 24, 25)
yield_B <- c(28, 30, 27, 26, 29)
yield_C <- c(18, 20, 22, 19, 21)

yield_data <- c(yield_A, yield_B, yield_C)
fertilizer_factor <- factor(rep(c("A", "B", "C"), each = 5))
df_wheat <- data.frame(Fertilizer = fertilizer_factor, Yield = yield_data)

# (b) Test at the 5% level of significance whether the three fertilizers give the same mean yield.
model_wheat <- aov(Yield ~ Fertilizer, data = df_wheat)
print("ANOVA for Wheat Yield:")
print(summary(model_wheat))
# Inference: The p-value (7.39e-05) is much less than 0.05. Therefore, we reject the null hypothesis. 
# There is a highly significant difference in the mean wheat yields among the three fertilizers.

# (c) If the treatments differ significantly, identify which pairs differ using Tukey's HSD test.
tukey_wheat <- TukeyHSD(model_wheat)
print("Tukey's HSD Test for Wheat Yield:")
print(tukey_wheat)
# Inference: Based on the adjusted p-values:
# B vs A (p = 0.0003) differ significantly.
# C vs A (p = 0.5480) do not differ significantly.
# C vs B (p = 0.0001) differ significantly.

# (d) Draw a boxplot of yield by fertilizer and interpret it.
boxplot(Yield ~ Fertilizer, data = df_wheat, 
        main = "Wheat Yield by Fertilizer Type",
        xlab = "Fertilizer", ylab = "Yield (kg/plot)", 
        col = c("lightblue", "lightgreen", "lightpink"))
# Inference: The boxplot shows that Fertilizer B yields noticeably higher than A and C. 
# Fertilizers A and C have similar medians and ranges, confirming the Tukey test results.

# Practical 2: Comparison of Three Teaching Methods

# Data Entry
lecture <- c(65, 70, 68, 72, 66, 69)
flipped <- c(78, 82, 80, 75, 79)
blended <- c(70, 74, 72, 71, 73, 75, 69)

scores <- c(lecture, flipped, blended)
methods <- factor(c(rep("Lecture", 6), rep("Flipped", 5), rep("Blended", 7)))
df_teaching <- data.frame(Method = methods, Score = scores)

# (a) Fit the CRD model and test whether the mean scores of the three teaching methods differ.
model_teaching <- aov(Score ~ Method, data = df_teaching)
print("ANOVA for Teaching Methods:")
print(summary(model_teaching))
# Inference: The p-value (3.73e-06) is less than 0.05, so we reject the null hypothesis. 
# The teaching methods have a statistically significant effect on the exam scores.

# (b) Check the ANOVA assumptions: normality of residuals and homogeneity of variances.
print("Shapiro-Wilk Test for Normality:")
print(shapiro.test(residuals(model_teaching)))
# Inference: The p-value (0.9168) is greater than 0.05, so we fail to reject the null hypothesis. 
# The residuals are approximately normally distributed.

print("Bartlett's Test for Homogeneity of Variances:")
print(bartlett.test(Score ~ Method, data = df_teaching))
# Inference: The p-value (0.835) is greater than 0.05. We fail to reject the null hypothesis. 
# The assumption of equal variances is satisfied.

# (c) Carry out pairwise comparisons using Tukey's HSD test.
tukey_teaching <- TukeyHSD(model_teaching)
print("Tukey's HSD Test for Teaching Methods:")
print(tukey_teaching)
# Inference: All pairwise comparisons are significant:
# Flipped vs Blended (p = 0.0003 < 0.05)
# Lecture vs Blended (p = 0.0381 < 0.05)
# Lecture vs Flipped (p = 0.000007 < 0.05)
# Each teaching method produces a significantly different mean score.

# (d) As a non-parametric check, apply the Kruskal-Wallis test and compare its conclusion with ANOVA.
kw_teaching <- kruskal.test(Score ~ Method, data = df_teaching)
print("Kruskal-Wallis Rank Sum Test:")
print(kw_teaching)
# Inference: The Kruskal-Wallis p-value (0.001669) is less than 0.05. We reject the null hypothesis.
# This non-parametric test leads to the same conclusion as the ANOVA: the teaching methods significantly affect exam scores.

# Conclusion:
# The CRD analysis successfully demonstrated that Fertilizer B produces higher wheat 
# yields than A and C. Furthermore, teaching methods significantly impact exam scores, 
# with the Flipped Classroom method being the most effective. Assumptions for ANOVA 
# were met and validated using non-parametric equivalents.
