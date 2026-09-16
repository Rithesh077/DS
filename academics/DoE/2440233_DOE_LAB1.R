# Title: Analysis of the Effect of Different Treatments on a Response Variable Using One-Way ANOVA
# Aim: To investigate whether different treatment levels have a significant effect on a response variable using
# One-Way ANOVA in R, followed by Critical Difference (CD) and Tukey's HSD test for pairwise
# comparison of treatment means.
#
# Objectives:
# Understand the purpose of one-way ANOVA. 
# Formulate null and alternative hypotheses. 
# Enter experimental data in R. 
# Visualize treatment-wise observations. 
# Perform one-way ANOVA using R. 
# Interpret the ANOVA table. 
# Make a statistical decision based on the p-value. 
# Perform a post-hoc test when the ANOVA is significant. 
# State the conclusion in the context of the experiment. 

# QUESTION 1: Effect of Four Different Fertilizers on Plant Growth
cat("QUESTION 1: Fertilizer Effect on Plant Growth\n")

# Factor: Fertilizer
# Levels: A, B, C, D
# Response variable: Plant growth (cm)
# Replications per treatment: 5
# Total observations: 20

# 1. Enter the data
A <- c(18, 20, 19, 21, 22)
B <- c(22, 24, 23, 25, 26)
C <- c(25, 27, 26, 28, 29)
D <- c(29, 31, 30, 32, 33)

growth <- c(A, B, C, D)
fertilizer <- factor(rep(c("A", "B", "C", "D"), each = 5))

# Create data frame
data_q1 <- data.frame(fertilizer, growth)

# 2. Descriptive statistics
cat("Descriptive Statistics\n")
mean_q1 <- aggregate(growth ~ fertilizer, data = data_q1, mean)
sd_q1 <- aggregate(growth ~ fertilizer, data = data_q1, sd)
print(mean_q1)
print(sd_q1)

# 3. Visualizing the Data
boxplot(growth ~ fertilizer, data = data_q1,
        xlab = "Fertilizer", ylab = "Plant Growth (cm)",
        main = "Q1: Plant Growth under Different Fertilizers",
        col = c("lightblue", "lightgreen", "pink", "yellow"))

# 4. Perform One-Way ANOVA
model_q1 <- aov(growth ~ fertilizer, data = data_q1)
cat("\nANOVA Table\n")
print(summary(model_q1))

# Extract p-value
p_val_q1 <- summary(model_q1)[[1]][["Pr(>F)"]][1]

# Dynamic Inference
cat("\nnference\n")
if (p_val_q1 < 0.05) {
  cat(sprintf("Since the p-value (%.4g) < 0.05, we reject the null hypothesis H0.\n", p_val_q1))
  cat("There is a statistically significant difference among the mean plant growth values for the four fertilizer treatments.\n")
} else {
  cat(sprintf("Since the p-value (%.4g) >= 0.05, we do not reject the null hypothesis H0.\n", p_val_q1))
  cat("There is no statistically significant difference among the mean plant growth values.\n")
}

# 5. Post-Hoc Analysis (Tukey HSD and Manual CD)
if (p_val_q1 < 0.05) {
  cat("\nPost-Hoc Analysis: Tukey's HSD\n")
  tukey_q1 <- TukeyHSD(model_q1)
  print(tukey_q1)
  
  # Manual Critical Difference (CD) Calculation
  # CD = t(alpha/2, df_error) * sqrt(2 * MSE / n)
  mse_q1 <- summary(model_q1)[[1]][["Mean Sq"]][2]
  df_error_q1 <- summary(model_q1)[[1]][["Df"]][2]
  t_val_q1 <- qt(1 - 0.05/2, df_error_q1)
  n_rep_q1 <- 5
  cd_q1 <- t_val_q1 * sqrt(2 * mse_q1 / n_rep_q1)
  
  cat(sprintf("\nCalculated Critical Difference (CD) at 5%% level: %.4f\n", cd_q1))
  cat("Any difference between two treatment means greater than the CD is considered significant.\n")
}

# 6. Checking ANOVA Assumptions
cat("\nChecking ANOVA Assumptions \n")
# Normality test
norm_q1 <- shapiro.test(residuals(model_q1))
cat(sprintf("Shapiro-Wilk Normality Test p-value: %.4g -> %s\n", 
            norm_q1$p.value, 
            ifelse(norm_q1$p.value >= 0.05, "Normality assumed.", "Normality violated.")))

# Homogeneity of variance
var_q1 <- bartlett.test(growth ~ fertilizer, data = data_q1)
cat(sprintf("Bartlett's Test of Homogeneity p-value: %.4g -> %s\n", 
            var_q1$p.value, 
            ifelse(var_q1$p.value >= 0.05, "Equal variances assumed.", "Variances are significantly different.")))

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(model_q1, main = "Q1 Diagnostic Plots")
par(mfrow = c(1, 1))

cat("\nConclusion\n")
cat("One-way ANOVA was performed to determine whether the mean plant growth differs among the four fertilizer treatments.\n")
cat(sprintf("Based on the obtained p-value (%.4g), the null hypothesis of equal treatment means was %s at the 5%% level of significance.\n", 
            p_val_q1, ifelse(p_val_q1 < 0.05, "rejected", "not rejected")))
cat(sprintf("Therefore, there %s sufficient statistical evidence to conclude that the fertilizer treatments differ in their mean plant growth.\n", 
            ifelse(p_val_q1 < 0.05, "is", "is not")))


# QUESTION 2: Effect of Machine Settings on Product Strength
cat("QUESTION 2: Effect of Machine Settings on Product Strength\n")

# 1. Enter the data
T150 <- c(42, 45, 44, 43, 46)
T160 <- c(48, 50, 47, 49, 51)
T170 <- c(53, 55, 54, 56, 52)
T180 <- c(55, 57, 56, 58, 54)

strength <- c(T150, T160, T170, T180)
temperature <- factor(rep(c("150°C", "160°C", "170°C", "180°C"), each = 5))

data_q2 <- data.frame(temperature, strength)

# 2. Descriptive statistics
cat("Descriptive Statistics\n")
mean_q2 <- aggregate(strength ~ temperature, data = data_q2, mean)
sd_q2 <- aggregate(strength ~ temperature, data = data_q2, sd)
print(mean_q2)
print(sd_q2)

# 3. Visualizing the Data
boxplot(strength ~ temperature, data = data_q2,
        xlab = "Temperature Setting", ylab = "Product Strength",
        main = "Q2: Product Strength under Different Temperatures",
        col = c("lightblue", "lightgreen", "pink", "yellow"))

# 4. Perform One-Way ANOVA
model_q2 <- aov(strength ~ temperature, data = data_q2)
cat("\nANOVA Table\n")
print(summary(model_q2))

# Extract p-value
p_val_q2 <- summary(model_q2)[[1]][["Pr(>F)"]][1]

# Dynamic Inference
cat("\nInference\n")
if (p_val_q2 < 0.05) {
  cat(sprintf("Since the p-value (%.4g) < 0.05, we reject the null hypothesis H0.\n", p_val_q2))
  cat("There is a statistically significant difference among the mean product strength values for the four temperatures.\n")
} else {
  cat(sprintf("Since the p-value (%.4g) >= 0.05, we do not reject the null hypothesis H0.\n", p_val_q2))
  cat("There is no statistically significant difference among the mean product strength values.\n")
}

# 5. Post-Hoc Analysis
if (p_val_q2 < 0.05) {
  cat("\nPost-Hoc Analysis: Tukey's HSD\n")
  tukey_q2 <- TukeyHSD(model_q2)
  print(tukey_q2)
}

# 6. Checking ANOVA Assumptions
cat("\nChecking ANOVA Assumptions\n")
norm_q2 <- shapiro.test(residuals(model_q2))
cat(sprintf("Shapiro-Wilk Normality Test p-value: %.4g -> %s\n", 
            norm_q2$p.value, ifelse(norm_q2$p.value >= 0.05, "Normality assumed.", "Normality violated.")))

var_q2 <- bartlett.test(strength ~ temperature, data = data_q2)
cat(sprintf("Bartlett's Test of Homogeneity p-value: %.4g -> %s\n", 
            var_q2$p.value, ifelse(var_q2$p.value >= 0.05, "Equal variances assumed.", "Variances are significantly different.")))

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(model_q2, main = "Q2 Diagnostic Plots")
par(mfrow = c(1, 1))

cat("\nConclusion\n")
cat("One-way ANOVA was performed to determine whether machine temperature affects the strength of a manufactured product.\n")
cat(sprintf("Based on the obtained p-value (%.4g), the null hypothesis was %s.\n", p_val_q2, ifelse(p_val_q2 < 0.05, "rejected", "not rejected")))


# QUESTION 3: Effect of feed on weight gain (chickwts dataset)
cat("QUESTION 3: Effect of Feed on Weight Gain (chickwts)\n")

# 1. Enter the data
data("chickwts")
data_q3 <- chickwts

# 2. Descriptive statistics
cat("Descriptive Statistics\n")
mean_q3 <- aggregate(weight ~ feed, data = data_q3, mean)
sd_q3 <- aggregate(weight ~ feed, data = data_q3, sd)
print(mean_q3)
print(sd_q3)

# 3. Visualizing the Data
boxplot(weight ~ feed, data = data_q3,
        xlab = "Feed Type", ylab = "Chick Weight (grams)",
        main = "Q3: Chick Weights under Different Feed Supplements",
        col = rainbow(6))

# 4. Perform One-Way ANOVA
model_q3 <- aov(weight ~ feed, data = data_q3)
cat("\nANOVA Table\n")
print(summary(model_q3))

p_val_q3 <- summary(model_q3)[[1]][["Pr(>F)"]][1]

# Dynamic Inference
cat("\nInference\n")
if (p_val_q3 < 0.05) {
  cat(sprintf("Since the p-value (%.4g) < 0.05, we reject the null hypothesis H0.\n", p_val_q3))
  cat("There is a statistically significant difference among the mean chick weights for the feed supplements.\n")
} else {
  cat(sprintf("Since the p-value (%.4g) >= 0.05, we do not reject the null hypothesis H0.\n", p_val_q3))
  cat("There is no statistically significant difference among the mean chick weights.\n")
}

# 5. Post-Hoc Analysis
if (p_val_q3 < 0.05) {
  cat("\nPost-Hoc Analysis: Tukey's HSD\n")
  tukey_q3 <- TukeyHSD(model_q3)
  print(tukey_q3)
}

# 6. Checking ANOVA Assumptions
cat("\nChecking ANOVA Assumptions\n")
norm_q3 <- shapiro.test(residuals(model_q3))
cat(sprintf("Shapiro-Wilk Normality Test p-value: %.4g -> %s\n", 
            norm_q3$p.value, ifelse(norm_q3$p.value >= 0.05, "Normality assumed.", "Normality violated.")))

var_q3 <- bartlett.test(weight ~ feed, data = data_q3)
cat(sprintf("Bartlett's Test of Homogeneity p-value: %.4g -> %s\n", 
            var_q3$p.value, ifelse(var_q3$p.value >= 0.05, "Equal variances assumed.", "Variances are significantly different.")))

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(model_q3, main = "Q3 Diagnostic Plots")
par(mfrow = c(1, 1))

cat("\nconclusion\n")
cat("One-way ANOVA was performed to examine whether the type of feed supplement significantly affects chick weight.\n")
cat(sprintf("Based on the obtained p-value (%.4g), the null hypothesis was %s.\n", p_val_q3, ifelse(p_val_q3 < 0.05, "rejected", "not rejected")))
