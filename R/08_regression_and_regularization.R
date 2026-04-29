# Regression and Regularization Artifact
# Demonstrates ordinary least squares, ridge regression, and lasso regression
# on the built-in mtcars dataset. Outputs are written to outputs/.

set.seed(123)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

soft_threshold <- function(value, penalty) {
  sign(value) * max(abs(value) - penalty, 0)
}

fit_ridge <- function(x, y, lambda) {
  design <- cbind(intercept = 1, x)
  penalty <- diag(ncol(design))
  penalty[1, 1] <- 0
  solve(t(design) %*% design + lambda * penalty, t(design) %*% y)
}

predict_ridge <- function(x, coefficients) {
  as.vector(cbind(intercept = 1, x) %*% coefficients)
}

fit_lasso <- function(x, y, lambda, max_iter = 5000, tolerance = 1e-7) {
  y_mean <- mean(y)
  y_centered <- y - y_mean
  beta <- rep(0, ncol(x))
  x_norm <- colSums(x^2)

  for (iteration in seq_len(max_iter)) {
    previous_beta <- beta

    for (j in seq_along(beta)) {
      partial_residual <- y_centered - as.vector(x %*% beta) + x[, j] * beta[j]
      beta[j] <- soft_threshold(sum(x[, j] * partial_residual), lambda) / x_norm[j]
    }

    if (max(abs(beta - previous_beta)) < tolerance) {
      break
    }
  }

  c(intercept = y_mean, beta)
}

predict_lasso <- function(x, coefficients) {
  as.vector(coefficients[1] + x %*% coefficients[-1])
}

standardize_from_train <- function(train_x, validation_x, test_x) {
  center <- colMeans(train_x)
  scale_values <- apply(train_x, 2, sd)

  list(
    train = scale(train_x, center = center, scale = scale_values),
    validation = scale(validation_x, center = center, scale = scale_values),
    test = scale(test_x, center = center, scale = scale_values)
  )
}

outputs_dir <- "outputs"
figures_dir <- file.path(outputs_dir, "figures")
tables_dir <- file.path(outputs_dir, "tables")
ensure_dir(figures_dir)
ensure_dir(tables_dir)

data <- mtcars
target <- "mpg"
predictors <- c("wt", "hp", "disp", "qsec")

sample_order <- sample(seq_len(nrow(data)))
train_index <- sample_order[1:20]
validation_index <- sample_order[21:26]
test_index <- sample_order[27:nrow(data)]

train_data <- data[train_index, c(target, predictors)]
validation_data <- data[validation_index, c(target, predictors)]
test_data <- data[test_index, c(target, predictors)]

x_scaled <- standardize_from_train(
  as.matrix(train_data[predictors]),
  as.matrix(validation_data[predictors]),
  as.matrix(test_data[predictors])
)

y_train <- train_data[[target]]
y_validation <- validation_data[[target]]
y_test <- test_data[[target]]

ols_model <- lm(mpg ~ wt + hp + disp + qsec, data = train_data)
ols_validation_predictions <- predict(ols_model, newdata = validation_data)
ols_test_predictions <- predict(ols_model, newdata = test_data)

lambda_grid <- exp(seq(log(0.01), log(100), length.out = 60))

ridge_coefficients <- sapply(lambda_grid, function(lambda) {
  fit_ridge(x_scaled$train, y_train, lambda)
})
ridge_validation_rmse <- vapply(seq_along(lambda_grid), function(index) {
  predictions <- predict_ridge(x_scaled$validation, ridge_coefficients[, index])
  rmse(y_validation, predictions)
}, numeric(1))
best_ridge_index <- which.min(ridge_validation_rmse)
best_ridge_lambda <- lambda_grid[best_ridge_index]
best_ridge_coefficients <- ridge_coefficients[, best_ridge_index]
ridge_test_predictions <- predict_ridge(x_scaled$test, best_ridge_coefficients)

lasso_coefficients <- sapply(lambda_grid, function(lambda) {
  fit_lasso(x_scaled$train, y_train, lambda)
})
lasso_validation_rmse <- vapply(seq_along(lambda_grid), function(index) {
  predictions <- predict_lasso(x_scaled$validation, lasso_coefficients[, index])
  rmse(y_validation, predictions)
}, numeric(1))
best_lasso_index <- which.min(lasso_validation_rmse)
best_lasso_lambda <- lambda_grid[best_lasso_index]
best_lasso_coefficients <- lasso_coefficients[, best_lasso_index]
lasso_test_predictions <- predict_lasso(x_scaled$test, best_lasso_coefficients)

model_summary <- data.frame(
  model = c("OLS", "Ridge", "Lasso"),
  selected_lambda = c(NA, best_ridge_lambda, best_lasso_lambda),
  validation_rmse = c(
    rmse(y_validation, ols_validation_predictions),
    min(ridge_validation_rmse),
    min(lasso_validation_rmse)
  ),
  test_rmse = c(
    rmse(y_test, ols_test_predictions),
    rmse(y_test, ridge_test_predictions),
    rmse(y_test, lasso_test_predictions)
  ),
  test_mae = c(
    mae(y_test, ols_test_predictions),
    mae(y_test, ridge_test_predictions),
    mae(y_test, lasso_test_predictions)
  ),
  nonzero_coefficients = c(
    length(coef(ols_model)) - 1,
    sum(abs(best_ridge_coefficients[-1]) > 1e-8),
    sum(abs(best_lasso_coefficients[-1]) > 1e-8)
  )
)

coefficient_summary <- data.frame(
  predictor = predictors,
  ols = coef(ols_model)[predictors],
  ridge = as.vector(best_ridge_coefficients[-1]),
  lasso = as.vector(best_lasso_coefficients[-1])
)

write.csv(
  model_summary,
  file.path(tables_dir, "regression_regularization_summary.csv"),
  row.names = FALSE
)
write.csv(
  coefficient_summary,
  file.path(tables_dir, "regression_regularization_coefficients.csv"),
  row.names = FALSE
)
saveRDS(
  list(
    model_summary = model_summary,
    coefficient_summary = coefficient_summary,
    lambda_grid = lambda_grid,
    ridge_validation_rmse = ridge_validation_rmse,
    lasso_validation_rmse = lasso_validation_rmse,
    ridge_coefficients = ridge_coefficients,
    lasso_coefficients = lasso_coefficients
  ),
  file.path(tables_dir, "regression_regularization_artifact.rds")
)

png(
  file.path(figures_dir, "regression_regularization_coefficients.png"),
  width = 1100,
  height = 520
)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 1), oma = c(0, 0, 2, 0))

matplot(
  log(lambda_grid),
  t(ridge_coefficients[-1, ]),
  type = "l",
  lty = 1,
  lwd = 2,
  xlab = "log(lambda)",
  ylab = "standardized coefficient",
  main = "Ridge coefficient paths"
)
abline(v = log(best_ridge_lambda), col = "gray40", lty = 2)
legend("topright", legend = predictors, col = seq_along(predictors), lty = 1, cex = 0.8)

matplot(
  log(lambda_grid),
  t(lasso_coefficients[-1, ]),
  type = "l",
  lty = 1,
  lwd = 2,
  xlab = "log(lambda)",
  ylab = "standardized coefficient",
  main = "Lasso coefficient paths"
)
abline(v = log(best_lasso_lambda), col = "gray40", lty = 2)
legend("topright", legend = predictors, col = seq_along(predictors), lty = 1, cex = 0.8)

mtext("Regression and Regularization on mtcars", outer = TRUE, cex = 1.2)
dev.off()

cat("Regression and regularization artifact complete.\n\n")
cat("Dataset: mtcars\n")
cat("Target: mpg\n")
cat("Predictors:", paste(predictors, collapse = ", "), "\n\n")
print(model_summary)
cat("\nArtifacts written:\n")
cat("- outputs/figures/regression_regularization_coefficients.png\n")
cat("- outputs/tables/regression_regularization_summary.csv\n")
cat("- outputs/tables/regression_regularization_coefficients.csv\n")
cat("- outputs/tables/regression_regularization_artifact.rds\n")
