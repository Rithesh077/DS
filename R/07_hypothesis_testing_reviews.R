# 07_hypothesis_testing_reviews.R
# Hypothesis Testing on Amazon Product Reviews
# Objective: Test whether mean ratings differ between high- and low-helpfulness reviews # nolint

library(dplyr)
library(ggplot2)
library(car)
library(rstatix)

# Data loading
zip_file <- "data/Reviews.csv.zip"
csv_file <- unzip(zip_file, list = TRUE)$Name[1]
reviews <- read.csv(unz(zip_file, csv_file))

# Data preparation and cleaning
df <- reviews %>%
    select(HelpfulnessNumerator, HelpfulnessDenominator, Score) %>% # nolint
    filter(HelpfulnessDenominator > 0) %>%
    mutate(
        helpfulness_ratio = HelpfulnessNumerator / HelpfulnessDenominator, # nolint
        group = ifelse(helpfulness_ratio >= 0.5, "High_Helpfulness", "Low_Helpfulness") # nolint
    ) %>% # nolint
    na.omit()

# Visualization
plot <- ggplot(df, aes(x = group, y = Score, fill = group)) +
    geom_boxplot() + # nolint
    theme_minimal() +
    labs(
        title = "Ratings by Review Helpfulness", # nolint
        x = "Helpfulness Group", y = "Score"
    ) # nolint

ggsave("outputs/figures/reviews_helpfulness_boxplot.png", plot)

# Assumption checks
set.seed(123) # reproducibility
normality <- df %>%
    group_by(group) %>% # nolint
    sample_n(min(5000, n()), replace = FALSE) %>%
    shapiro_test(Score)
variance <- leveneTest(Score ~ group, data = df)

# Hypothesis testing (t-test)
t_test_res <- t.test(Score ~ group, data = df, var.equal = TRUE)

# Non-parametric alternative
wilcox_res <- wilcox.test(Score ~ group, data = df)


saveRDS(list(
    normality = normality, # nolint
    variance = variance,
    t_test = t_test_res,
    wilcox = wilcox_res
), file = "outputs/tables/hypothesis_test_results.rds")

print("Analysis complete. Results saved to outputs/tables/")
# Note: Next steps could include more detailed subgroup analyses or regression modeling to control for confounders. # nolint
