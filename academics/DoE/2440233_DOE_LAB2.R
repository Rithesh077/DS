# title
# design of experiments lab assignment two

# question one
# use toothgrowth data to study the effect of supplement and dose on tooth length
# test the effect of dose alone
# test whether supplement effect depends on dose
# compare the models

# introduction
# two way anova is used to study the effects of two factors on a response variable
# the response variable is tooth length
# the factors are supplement type and dose

# data overview

data_one <- ToothGrowth

data_one$supp <- factor(data_one$supp)
data_one$dose <- factor(data_one$dose)

print("data overview")
print(head(data_one))
print(summary(data_one))

# model without interaction

model_add <- aov(len ~ supp + dose, data = data_one)

print("anova without interaction")
print(summary(model_add))

# model with interaction

model_int <- aov(len ~ supp * dose, data = data_one)

print("anova with interaction")
print(summary(model_int))

# interpretation of dose

p_dose <- summary(model_add)[[1]]["dose", "Pr(>F)"]

if (p_dose < 0.05) {
  print("dose has a significant effect on tooth length")
} else {
  print("dose does not have a significant effect on tooth length")
}

# interpretation of interaction

p_interaction <- summary(model_int)[[1]]["supp:dose", "Pr(>F)"]

if (p_interaction < 0.05) {
  print("the effect of supplement depends significantly on dose")
} else {
  print("the effect of supplement does not depend significantly on dose")
}

# model comparison

print("comparison of the two models")
print(anova(model_add, model_int))

# tukey test for dose

print("tukey test for dose")
print(TukeyHSD(model_add, "dose"))

# critical difference for dose

mse <- summary(model_add)[[1]]["Residuals", "Mean Sq"]
error_df <- summary(model_add)[[1]]["Residuals", "Df"]
n <- 20

critical_difference <- qt(0.975, error_df) * sqrt(2 * mse / n)

print("critical difference for dose")
print(critical_difference)

# dose means

dose_means <- with(data_one, tapply(len, dose, mean))

print("dose means")
print(dose_means)

# interpretation of critical difference

if (max(dose_means) - min(dose_means) > critical_difference) {
  print("at least one pair of dose means differs significantly")
} else {
  print("the dose means do not differ significantly based on the critical difference")
}

# conclusion
# the final conclusion is based on the dose p value and interaction p value
# a significant dose p value indicates that tooth length differs across dose levels
# a significant interaction p value indicates that supplement and dose work together


# question two
# study the effect of teaching method and prior training on test scores
# perform a two way anova including interaction
# state the hypotheses
# construct the anova table
# test the significance of each effect at the five percent level
# interpret the main effects and interaction
# explain why main effects need cautious interpretation when interaction is significant

# introduction
# the response variable is test score
# the two factors are teaching method and training
# each combination contains three observations in the supplied data

# data overview

data_two <- data.frame(
  method = factor(rep(c("a", "b", "c"), each = 6)),
  training = factor(rep(c("no", "yes"), each = 3, times = 3)),
  score = c(
    55, 57, 58,
    65, 67, 67,
    60, 62, 63,
    72, 74, 76,
    70, 72, 70,
    71, 73, 74
  )
)

print("data overview")
print(data_two)
print(summary(data_two))

# two way anova with interaction

model_two <- aov(score ~ method * training, data = data_two)

anova_two <- summary(model_two)[[1]]

print("anova table")
print(anova_two)

# hypotheses for teaching method
# null hypothesis
# all teaching method means are equal
# alternative hypothesis
# at least one teaching method mean is different

# hypotheses for training
# null hypothesis
# the two training means are equal
# alternative hypothesis
# the training means are different

# hypotheses for interaction
# null hypothesis
# there is no interaction between teaching method and training
# alternative hypothesis
# there is an interaction between teaching method and training

# significance of teaching method

p_method <- anova_two[1, "Pr(>F)"]

if (p_method < 0.05) {
  print("teaching method has a significant effect on test scores")
} else {
  print("teaching method does not have a significant effect on test scores")
}

# significance of training

p_training <- anova_two[2, "Pr(>F)"]

if (p_training < 0.05) {
  print("training has a significant effect on test scores")
} else {
  print("training does not have a significant effect on test scores")
}

# significance of interaction

p_method_training <- anova_two[3, "Pr(>F)"]

if (p_method_training < 0.05) {
  print("the interaction between teaching method and training is significant")
} else {
  print("the interaction between teaching method and training is not significant")
}

# cell means

cell_means <- with(
  data_two,
  tapply(score, list(method, training), mean)
)

print("cell means")
print(cell_means)

# interaction plot

interaction.plot(
  data_two$method,
  data_two$training,
  data_two$score,
  xlab = "teaching method",
  ylab = "mean score",
  trace.label = "training"
)

# interpretation of interaction

if (p_method_training < 0.05) {
  print("the effect of teaching method changes with training level")
  print("the main effects should be interpreted cautiously")
  print("the cell means should be examined when interpreting the results")
} else {
  print("the effect of teaching method does not significantly change with training level")
  print("the main effects can be interpreted separately")
}

# conclusion
# the anova results show whether teaching method training and their interaction
# significantly affect test scores
# when interaction is significant the combinations of the two factors
# should be considered instead of interpreting the main effects alone
