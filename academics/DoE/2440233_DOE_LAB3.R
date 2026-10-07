# random effects and mixed effects models


# question 1
# random effects model with interaction

machine <- factor(rep(1:3, each = 8))

operator <- factor(
  rep(rep(1:4, each = 2), 3)
)

yield <- c(
  51, 49, 54, 56, 48, 50, 53, 55,
  57, 59, 61, 60, 54, 56, 58, 57,
  48, 50, 52, 54, 45, 47, 50, 51
)

data_q1 <- data.frame(
  machine,
  operator,
  yield
)

print("question 1 data")
print(data_q1)

# anova model

model_q1 <- aov(
  yield ~ machine * operator,
  data = data_q1
)

anova_q1 <- summary(model_q1)[[1]]

print("question 1 anova table")
print(anova_q1)

# mean squares

ms_machine <- anova_q1[1, "Mean Sq"]
ms_operator <- anova_q1[2, "Mean Sq"]
ms_interaction <- anova_q1[3, "Mean Sq"]
ms_error <- anova_q1[4, "Mean Sq"]

# degrees of freedom

df_machine <- anova_q1[1, "Df"]
df_operator <- anova_q1[2, "Df"]
df_interaction <- anova_q1[3, "Df"]
df_error <- anova_q1[4, "Df"]

# random effects f tests

f_machine <- ms_machine / ms_interaction

f_operator <- ms_operator / ms_interaction

f_interaction <- ms_interaction / ms_error

# p values

p_machine <- pf(
  f_machine,
  df_machine,
  df_interaction,
  lower.tail = FALSE
)

p_operator <- pf(
  f_operator,
  df_operator,
  df_interaction,
  lower.tail = FALSE
)

p_interaction <- pf(
  f_interaction,
  df_interaction,
  df_error,
  lower.tail = FALSE
)

print("machine f value")
print(f_machine)

print("machine p value")
print(p_machine)

if (p_machine < 0.05) {
  print("machine variance component is significant")
} else {
  print("machine variance component is not significant")
}

print("operator f value")
print(f_operator)

print("operator p value")
print(p_operator)

if (p_operator < 0.05) {
  print("operator variance component is significant")
} else {
  print("operator variance component is not significant")
}

print("machine operator interaction f value")
print(f_interaction)

print("machine operator interaction p value")
print(p_interaction)

if (p_interaction < 0.05) {
  print("machine operator interaction variance component is significant")
} else {
  print("machine operator interaction variance component is not significant")
}

# variance components

a <- 3
b <- 4
n <- 2

sigma_error_q1 <- ms_error

sigma_interaction_q1 <- (
  ms_interaction - ms_error
) / n

sigma_machine_q1 <- (
  ms_machine - ms_interaction
) / (b * n)

sigma_operator_q1 <- (
  ms_operator - ms_interaction
) / (a * n)

print("estimated error variance")
print(sigma_error_q1)

print("estimated machine variance")
print(sigma_machine_q1)

print("estimated operator variance")
print(sigma_operator_q1)

print("estimated machine operator interaction variance")
print(sigma_interaction_q1)


# question 2
# random effects model without interaction

factory <- factor(rep(1:4, each = 3))

shift <- factor(rep(1:3, times = 4))

productivity <- c(
  42, 45, 44,
  51, 53, 52,
  38, 40, 39,
  47, 49, 48
)

data_q2 <- data.frame(
  factory,
  shift,
  productivity
)

print("question 2 data")
print(data_q2)

# anova model without interaction

model_q2 <- aov(
  productivity ~ factory + shift,
  data = data_q2
)

anova_q2 <- summary(model_q2)[[1]]

print("question 2 anova table")
print(anova_q2)

# mean squares

ms_factory <- anova_q2[1, "Mean Sq"]
ms_shift <- anova_q2[2, "Mean Sq"]
ms_error_q2 <- anova_q2[3, "Mean Sq"]

# degrees of freedom

df_factory <- anova_q2[1, "Df"]
df_shift <- anova_q2[2, "Df"]
df_error_q2 <- anova_q2[3, "Df"]

# f tests

f_factory <- ms_factory / ms_error_q2

f_shift <- ms_shift / ms_error_q2

# p values

p_factory <- pf(
  f_factory,
  df_factory,
  df_error_q2,
  lower.tail = FALSE
)

p_shift <- pf(
  f_shift,
  df_shift,
  df_error_q2,
  lower.tail = FALSE
)

print("factory f value")
print(f_factory)

print("factory p value")
print(p_factory)

if (p_factory < 0.05) {
  print("factory variance component is significant")
} else {
  print("factory variance component is not significant")
}

print("shift f value")
print(f_shift)

print("shift p value")
print(p_shift)

if (p_shift < 0.05) {
  print("shift variance component is significant")
} else {
  print("shift variance component is not significant")
}

# variance components

a <- 4
b <- 3

sigma_error_q2 <- ms_error_q2

sigma_factory_q2 <- (
  ms_factory - ms_error_q2
) / b

sigma_shift_q2 <- (
  ms_shift - ms_error_q2
) / a

print("estimated error variance")
print(sigma_error_q2)

print("estimated factory variance")
print(sigma_factory_q2)

print("estimated shift variance")
print(sigma_shift_q2)


# question 3
# oats mixed effects model

# load nlme

if (!requireNamespace("nlme", quietly = TRUE)) {
  install.packages(
    "nlme",
    repos = "https://cloud.r-project.org"
  )
}

library(nlme)

# load oats data

data(Oats)

print("question 3 data")
print(head(Oats))

print("question 3 structure")
print(str(Oats))

print("question 3 summary")
print(summary(Oats))

# check column names

print("column names")
print(names(Oats))

# convert variables to factors

Oats$Block <- factor(Oats$Block)

Oats$Variety <- factor(Oats$Variety)

# nitro is the actual nitrogen variable in the Oats dataset

Oats$nitro <- factor(Oats$nitro)

# mixed effects model
# block and variety within block are random effects
# variety and nitro are fixed effects

model_q3 <- lme(
  fixed = yield ~ Variety * nitro,
  random = ~ 1 | Block / Variety,
  data = Oats,
  method = "REML"
)

print("question 3 mixed effects model")
print(summary(model_q3))

# anova table

anova_q3 <- anova(model_q3)

print("question 3 anova table")
print(anova_q3)

# variance components

print("question 3 variance components")
print(VarCorr(model_q3))

# p values

p_variety <- anova_q3[1, "p-value"]

p_nitro <- anova_q3[2, "p-value"]

p_interaction_q3 <- anova_q3[3, "p-value"]

print("variety p value")
print(p_variety)

if (p_variety < 0.05) {
  print("variety has a significant effect on oat yield")
} else {
  print("variety does not have a significant effect on oat yield")
}

print("nitrogen p value")
print(p_nitro)

if (p_nitro < 0.05) {
  print("nitrogen concentration has a significant effect on oat yield")
} else {
  print("nitrogen concentration does not have a significant effect on oat yield")
}

print("variety nitrogen interaction p value")
print(p_interaction_q3)

if (p_interaction_q3 < 0.05) {
  print("variety and nitrogen have a significant interaction")
} else {
  print("variety and nitrogen do not have a significant interaction")
}

# residuals

residuals_q3 <- residuals(model_q3)

fitted_q3 <- fitted(model_q3)

# residual plot

plot(
  fitted_q3,
  residuals_q3,
  xlab = "fitted values",
  ylab = "residuals",
  main = "residuals versus fitted values"
)

abline(h = 0)

# normal q q plot

qqnorm(
  residuals_q3,
  main = "normal q q plot"
)

qqline(residuals_q3)

# residual interpretation

print("residual interpretation")
print("residuals should be approximately centred around zero")
print("residuals should not show a clear systematic pattern")
print("normal q q plot points should approximately follow the reference line")

# final conclusions

print("question 3 conclusion")
print("the oats experiment is a split plot experiment")
print("block is treated as a random effect")
print("variety within block represents the whole plot random variation")
print("variety and nitrogen are included as fixed effects")
print("residual variance represents subplot level unexplained variation")