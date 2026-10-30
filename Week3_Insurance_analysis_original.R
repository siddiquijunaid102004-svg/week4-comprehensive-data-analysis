# ================================================================
# Week 3: Statistical Analysis and Predictive Modeling Using R
# Dataset: Kaggle Medical Cost Personal Dataset (insurance.csv)
# Target: charges (continuous) -> regression
# Run from the project root: source("analysis.R")
# ================================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(2026)

# ---- 1. Packages ----
required_packages <- c("ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages) > 0) {
  message("Installing required package(s): ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(library(ggplot2))

# ---- 2. Project folders and input validation ----
dirs <- c("outputs", "outputs/plots", "outputs/tables", "outputs/model", "report")
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

input_file <- file.path("01_Datasets", "insurance.csv")
if (!file.exists(input_file)) {
  stop(
    "Dataset not found at 'data/insurance.csv'. Download insurance.csv from ",
    "https://www.kaggle.com/datasets/mirichoi0218/insurance and place it in the data folder."
  )
}

# ---- 3. Import and validate data ----
raw_data <- read.csv(input_file, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A"))
expected_cols <- c("age", "sex", "bmi", "children", "smoker", "region", "charges")
missing_cols <- setdiff(expected_cols, names(raw_data))
if (length(missing_cols) > 0) {
  stop("Dataset is missing expected column(s): ", paste(missing_cols, collapse = ", "),
       ". Check that the correct Kaggle insurance.csv file was downloaded.")
}
data <- raw_data[, expected_cols]
initial_rows <- nrow(data)
initial_cols <- ncol(data)
duplicate_rows <- sum(duplicated(data))
missing_by_column <- data.frame(
  variable = names(data),
  missing_count = vapply(data, function(x) sum(is.na(x)), numeric(1)),
  missing_percent = round(100 * vapply(data, function(x) mean(is.na(x)), numeric(1)), 3)
)
write.csv(missing_by_column, "outputs/tables/missing_values_summary.csv", row.names = FALSE)

# Standardize categorical values conservatively
for (v in c("sex", "smoker", "region")) {
  data[[v]] <- tolower(trimws(as.character(data[[v]])))
}
data$sex <- factor(data$sex)
data$smoker <- factor(data$smoker)
data$region <- factor(data$region)

# Flag implausible values rather than silently deleting them
invalid_age <- !is.na(data$age) & (data$age < 0 | data$age > 120)
invalid_bmi <- !is.na(data$bmi) & (data$bmi <= 0 | data$bmi > 100)
invalid_children <- !is.na(data$children) & (data$children < 0 | data$children != floor(data$children))
invalid_charges <- !is.na(data$charges) & data$charges < 0
invalid_rows <- invalid_age | invalid_bmi | invalid_children | invalid_charges

quality_summary <- data.frame(
  item = c("Rows imported", "Columns imported", "Exact duplicate rows", "Rows with invalid numeric values",
           "Rows containing at least one missing value", "Rows retained after complete-case filtering"),
  value = c(initial_rows, initial_cols, duplicate_rows, sum(invalid_rows),
            sum(!complete.cases(data)), sum(complete.cases(data) & !invalid_rows))
)
write.csv(quality_summary, "outputs/tables/data_quality_summary.csv", row.names = FALSE)

# Remove exact duplicate rows; exclude records with missing or clearly invalid core values.
# These decisions are reported so the analysis is transparent.
data <- data[!duplicated(data), ]
valid_rows <- !is.na(data$age) & !is.na(data$bmi) & !is.na(data$children) &
  !is.na(data$charges) & !is.na(data$sex) & !is.na(data$smoker) & !is.na(data$region) &
  data$age >= 0 & data$age <= 120 & data$bmi > 0 & data$bmi <= 100 &
  data$children >= 0 & data$children == floor(data$children) & data$charges >= 0
data <- droplevels(data[valid_rows, ])
if (nrow(data) < 50) stop("Too few valid rows remain after cleaning; inspect the dataset before modeling.")
if (length(unique(data$charges)) < 2) stop("Target variable 'charges' does not vary.")

# ---- 4. Descriptive statistics ----
numeric_vars <- c("age", "bmi", "children", "charges")
desc_list <- lapply(numeric_vars, function(v) {
  x <- data[[v]]
  data.frame(variable = v, n = length(x), mean = mean(x), median = median(x),
             sd = sd(x), min = min(x), q1 = unname(quantile(x, 0.25)),
             q3 = unname(quantile(x, 0.75)), max = max(x))
})
descriptive_stats <- do.call(rbind, desc_list)
descriptive_stats[-1] <- lapply(descriptive_stats[-1], function(x) round(as.numeric(x), 4))
write.csv(descriptive_stats, "outputs/tables/descriptive_statistics.csv", row.names = FALSE)

for (v in c("sex", "smoker", "region", "children")) {
  freq <- as.data.frame(table(data[[v]], useNA = "ifany"))
  names(freq) <- c("category", "count")
  freq$percent <- round(100 * freq$count / sum(freq$count), 2)
  write.csv(freq, paste0("outputs/tables/frequency_", v, ".csv"), row.names = FALSE)
}

cor_mat <- cor(data[, numeric_vars], use = "pairwise.complete.obs", method = "spearman")
write.csv(round(cor_mat, 4), "outputs/tables/spearman_correlation_matrix.csv")

# ---- 5. Exploratory visualizations ----
p1 <- ggplot(data, aes(x = charges)) +
  geom_histogram(bins = 35, color = "white", fill = "#2878B5") +
  labs(title = "Distribution of Medical Insurance Charges", x = "Charges (USD)", y = "Number of records") +
  theme_minimal(base_size = 12)
ggsave("outputs/plots/01_target_distribution.png", p1, width = 8, height = 5, dpi = 300)

p2 <- ggplot(data, aes(x = age, y = charges)) +
  geom_point(alpha = 0.55, color = "#2878B5") +
  geom_smooth(method = "lm", se = TRUE, color = "#D9534F") +
  labs(title = "Age and Medical Insurance Charges", x = "Age", y = "Charges (USD)") +
  theme_minimal(base_size = 12)
ggsave("outputs/plots/02_age_vs_charges.png", p2, width = 8, height = 5, dpi = 300)

p3 <- ggplot(data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.35) +
  labs(title = "Insurance Charges by Smoking Status", x = "Smoking status", y = "Charges (USD)") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave("outputs/plots/03_charges_by_smoker.png", p3, width = 8, height = 5, dpi = 300)

p4 <- ggplot(data, aes(x = region, y = charges, fill = region)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.35) +
  labs(title = "Insurance Charges by Region", x = "Region", y = "Charges (USD)") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave("outputs/plots/04_charges_by_region.png", p4, width = 8, height = 5, dpi = 300)

cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("variable_x", "variable_y", "spearman_rho")
p5 <- ggplot(cor_long, aes(x = variable_x, y = variable_y, fill = spearman_rho)) +
  geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", spearman_rho)), size = 4) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Spearman Correlation Matrix", x = NULL, y = NULL, fill = "Spearman\nrho") +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave("outputs/plots/05_correlation_heatmap.png", p5, width = 8, height = 6, dpi = 300)

p6 <- ggplot(data, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "BMI, Smoking Status and Charges", x = "BMI", y = "Charges (USD)", color = "Smoker") +
  theme_minimal(base_size = 12)
ggsave("outputs/plots/06_bmi_smoker_charges.png", p6, width = 8, height = 5, dpi = 300)

# ---- 6. Statistical hypothesis tests ----
# H0: age and charges have no monotonic association.
age_cor <- cor.test(data$age, data$charges, method = "spearman", exact = FALSE)

# H0: mean charges are equal for smokers and non-smokers.
if (nlevels(droplevels(data$smoker)) == 2) {
  smoker_test <- t.test(charges ~ smoker, data = data)  # Welch's t-test
  smoker_stat <- unname(smoker_test$statistic)
  smoker_p <- smoker_test$p.value
  smoker_method <- smoker_test$method
  smoker_estimate <- paste(names(smoker_test$estimate), round(smoker_test$estimate, 2), collapse = "; ")
} else {
  smoker_stat <- NA_real_; smoker_p <- NA_real_; smoker_method <- "Not run: smoker must have two levels"
  smoker_estimate <- "Not available"
}

# H0: average charges are equal across the observed regions.
region_test <- tryCatch(oneway.test(charges ~ region, data = data, var.equal = FALSE),
                        error = function(e) NULL)

# Shapiro-Wilk is applied to a reproducible sample of target values; it is a diagnostic,
# not a sole decision rule for selecting a regression model.
set.seed(2026)
target_sample <- sample(data$charges, min(5000, nrow(data)))
shapiro_result <- if (length(target_sample) >= 3 && length(target_sample) <= 5000) {
  shapiro.test(target_sample)
} else NULL

test_rows <- list(
  data.frame(test = "Spearman correlation: age vs charges",
             null_hypothesis = "No monotonic association between age and charges",
             statistic = unname(age_cor$statistic), p_value = age_cor$p.value,
             method = age_cor$method,
             decision_alpha_0_05 = ifelse(age_cor$p.value < 0.05, "Reject H0", "Fail to reject H0")),
  data.frame(test = "Welch two-sample t-test: charges by smoker",
             null_hypothesis = "Equal mean charges for smokers and non-smokers",
             statistic = smoker_stat, p_value = smoker_p, method = smoker_method,
             decision_alpha_0_05 = ifelse(is.na(smoker_p), "Not tested",
                                          ifelse(smoker_p < 0.05, "Reject H0", "Fail to reject H0"))),
  data.frame(test = "Welch one-way ANOVA: charges by region",
             null_hypothesis = "Equal mean charges across regions",
             statistic = if (is.null(region_test)) NA_real_ else unname(region_test$statistic),
             p_value = if (is.null(region_test)) NA_real_ else region_test$p.value,
             method = if (is.null(region_test)) "Not run" else region_test$method,
             decision_alpha_0_05 = if (is.null(region_test)) "Not tested" else
               ifelse(region_test$p.value < 0.05, "Reject H0", "Fail to reject H0")),
  data.frame(test = "Shapiro-Wilk test: target sample",
             null_hypothesis = "Sample values follow a normal distribution",
             statistic = if (is.null(shapiro_result)) NA_real_ else unname(shapiro_result$statistic),
             p_value = if (is.null(shapiro_result)) NA_real_ else shapiro_result$p.value,
             method = "Shapiro-Wilk (interpret alongside plots and sample size)",
             decision_alpha_0_05 = if (is.null(shapiro_result)) "Not tested" else
               ifelse(shapiro_result$p.value < 0.05, "Reject H0", "Fail to reject H0"))
)
test_results <- do.call(rbind, test_rows)
write.csv(test_results, "outputs/tables/hypothesis_test_results.csv", row.names = FALSE)

# ---- 7. Train/test split and cross-validation ----
# A simple random split is used because the records are independent rows rather than a time series.
set.seed(2026)
train_index <- sample(seq_len(nrow(data)), size = floor(0.80 * nrow(data))
)
train <- data[train_index, , drop = FALSE]
test <- data[-train_index, , drop = FALSE]
write.csv(data.frame(split = c("training", "testing"), rows = c(nrow(train), nrow(test)),
                     percentage = round(100 * c(nrow(train), nrow(test)) / nrow(data), 2)),
          "outputs/tables/train_test_split.csv", row.names = FALSE)

# Baseline prediction is the training-set mean; predictors are learned only from training data.
baseline_pred <- rep(mean(train$charges), nrow(test))
baseline_rmse <- sqrt(mean((test$charges - baseline_pred)^2))
baseline_mae <- mean(abs(test$charges - baseline_pred))
baseline_r2 <- 1 - sum((test$charges - baseline_pred)^2) / sum((test$charges - mean(test$charges))^2)

model_formula <- charges ~ age + sex + bmi + children + smoker + region + bmi:smoker
model <- lm(model_formula, data = train)
model_pred <- predict(model, newdata = test)
model_rmse <- sqrt(mean((test$charges - model_pred)^2))
model_mae <- mean(abs(test$charges - model_pred))
model_r2 <- 1 - sum((test$charges - model_pred)^2) / sum((test$charges - mean(test$charges))^2)

# Five-fold CV on training data only
set.seed(2026)
fold_id <- sample(rep(1:5, length.out = nrow(train)))
cv_results <- data.frame(fold = integer(), RMSE = numeric(), MAE = numeric(), R_squared = numeric())
for (k in 1:5) {
  cv_train <- train[fold_id != k, , drop = FALSE]
  cv_valid <- train[fold_id == k, , drop = FALSE]
  cv_model <- lm(model_formula, data = cv_train)
  cv_pred <- predict(cv_model, newdata = cv_valid)
  cv_rmse <- sqrt(mean((cv_valid$charges - cv_pred)^2))
  cv_mae <- mean(abs(cv_valid$charges - cv_pred))
  cv_r2 <- 1 - sum((cv_valid$charges - cv_pred)^2) /
    sum((cv_valid$charges - mean(cv_valid$charges))^2)
  cv_results <- rbind(cv_results, data.frame(fold = k, RMSE = cv_rmse, MAE = cv_mae, R_squared = cv_r2))
}
write.csv(cv_results, "outputs/tables/cross_validation_results.csv", row.names = FALSE)

performance <- data.frame(
  model = c("Baseline: training mean", "Multiple linear regression with BMI-smoking interaction"),
  RMSE = c(baseline_rmse, model_rmse),
  MAE = c(baseline_mae, model_mae),
  Test_R_squared = c(baseline_r2, model_r2)
)
performance[-1] <- lapply(performance[-1], function(x) round(x, 4))
write.csv(performance, "outputs/tables/model_performance.csv", row.names = FALSE)

# ---- 8. Coefficients, predictions and diagnostics ----
coef_table <- as.data.frame(summary(model)$coefficients)
coef_table$term <- rownames(coef_table)
rownames(coef_table) <- NULL
names(coef_table) <- c("estimate", "std_error", "t_value", "p_value", "term")
coef_table <- coef_table[, c("term", "estimate", "std_error", "t_value", "p_value")]
write.csv(coef_table, "outputs/tables/model_coefficients.csv", row.names = FALSE)

predictions <- data.frame(actual_charges = test$charges, predicted_charges = as.numeric(model_pred),
                          residual = test$charges - as.numeric(model_pred))
write.csv(predictions, "outputs/model/test_set_predictions.csv", row.names = FALSE)

diag_data <- data.frame(fitted = fitted(model), residuals = resid(model))
d1 <- ggplot(diag_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) + geom_hline(yintercept = 0, linetype = "dashed", color = "#D9534F") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted values", y = "Residuals") + theme_minimal(base_size = 12)
ggsave("outputs/plots/07_residuals_vs_fitted.png", d1, width = 8, height = 5, dpi = 300)

d2 <- ggplot(diag_data, aes(sample = residuals)) +
  stat_qq(alpha = 0.6) + stat_qq_line(color = "#D9534F") +
  labs(title = "Normal Q-Q Plot of Regression Residuals", x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 12)
ggsave("outputs/plots/08_residual_qq_plot.png", d2, width = 8, height = 5, dpi = 300)

d3 <- ggplot(predictions, aes(x = actual_charges, y = predicted_charges)) +
  geom_point(alpha = 0.6, color = "#2878B5") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#D9534F") +
  labs(title = "Actual vs Predicted Charges (Test Set)", x = "Actual charges (USD)", y = "Predicted charges (USD)") +
  theme_minimal(base_size = 12)
ggsave("outputs/plots/09_actual_vs_predicted.png", d3, width = 8, height = 5, dpi = 300)

# Multicollinearity proxy: variance inflation factors computed from model-matrix columns.
# This is a basic diagnostic; factor expansion can make interpretation less direct.
mm <- model.matrix(model)[, -1, drop = FALSE]
vif_values <- rep(NA_real_, ncol(mm))
names(vif_values) <- colnames(mm)
if (ncol(mm) >= 2) {
  for (j in seq_len(ncol(mm))) {
    yj <- mm[, j]
    others <- as.data.frame(mm[, -j, drop = FALSE])
    if (sd(yj) > 0) {
      aux_data <- cbind(yj = yj, others)
      aux <- lm(yj ~ ., data = aux_data)
      r2_aux <- summary(aux)$r.squared
      vif_values[j] <- if (1 - r2_aux <= 0) Inf else 1 / (1 - r2_aux)
    }
  }
}
write.csv(data.frame(model_matrix_term = names(vif_values), VIF = as.numeric(vif_values)),
          "outputs/tables/collinearity_diagnostic.csv", row.names = FALSE)

# ---- 9. Plain-text summary and session information ----
summary_lines <- c(
  "WEEK 3 STATISTICAL ANALYSIS AND PREDICTIVE MODELING",
  paste("Run timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Rows imported:", initial_rows),
  paste("Columns imported:", initial_cols),
  paste("Exact duplicate rows detected before cleaning:", duplicate_rows),
  paste("Rows in analysis dataset:", nrow(data)),
  paste("Training rows:", nrow(train), "| Testing rows:", nrow(test)),
  "",
  "MODEL PERFORMANCE (test set)",
  capture.output(print(performance, row.names = FALSE)),
  "",
  "HYPOTHESIS TEST RESULTS",
  capture.output(print(test_results, row.names = FALSE)),
  "",
  "Interpretation note: statistical association does not prove causation.",
  "This dataset is educational and should not be used for individual medical or insurance decisions."
)
writeLines(summary_lines, "outputs/analysis_run_summary.txt")
writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")

message("Analysis completed. Review outputs/analysis_run_summary.txt and all files under outputs/.")
