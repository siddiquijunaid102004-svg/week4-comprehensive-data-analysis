# ============================================================
# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: World Bank Fertility Rate (1960-2013 snapshot)
# Main analysis period: 2000-2011
# ============================================================

# 1. Setup ----------------------------------------------------
# This script uses base R only, so no extra package installation is needed.

file_path <- file.path("01_Datasets", "world_bank_fertility.csv")
output_dir <- "outputs"

if (!file.exists(file_path)) {
  stop("Dataset not found. Put world_bank_fertility.csv in the same folder as this R file.")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

data_raw <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

cat("Rows:", nrow(data_raw), "\n")
cat("Columns:", ncol(data_raw), "\n\n")
cat("Structure of raw data:\n")
str(data_raw)

# 2. Initial data inspection ---------------------------------
cat("\nFirst six rows:\n")
print(head(data_raw))

cat("\nSummary of raw data:\n")
print(summary(data_raw))

# 3. Remove columns that are completely missing --------------
all_missing_cols <- names(data_raw)[sapply(data_raw, function(x) all(is.na(x)))]
cat("\nColumns that are completely missing:\n")
print(all_missing_cols)

# In this dataset snapshot, 2012 and 2013 are entirely missing.
data <- data_raw[, !(names(data_raw) %in% all_missing_cols)]

# 4. Select useful variables ---------------------------------
year_cols <- as.character(2000:2011)
keep_cols <- c("Country Name", "Country Code", year_cols)
data <- data[, keep_cols]

# Make sure all year columns are numeric.
for (col in year_cols) {
  data[[col]] <- as.numeric(data[[col]])
}

# 5. Missing-value analysis ----------------------------------
missing_summary <- data.frame(
  Variable = names(data),
  Missing_Values = sapply(data, function(x) sum(is.na(x)))
)
missing_summary$Missing_Percent <- round(
  100 * missing_summary$Missing_Values / nrow(data), 2
)

cat("\nMissing-value summary:\n")
print(missing_summary)
write.csv(missing_summary,
          file.path(output_dir, "missing_values_summary.csv"),
          row.names = FALSE)

# 6. Outlier detection using the IQR method -------------------
# Outliers are checked on the observed 2010 values BEFORE imputation.
x2010 <- data$`2010`
observed_2010 <- x2010[!is.na(x2010)]

Q1 <- quantile(observed_2010, 0.25)
Q3 <- quantile(observed_2010, 0.75)
IQR_value <- IQR(observed_2010)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outlier_flag <- !is.na(x2010) & (x2010 < lower_bound | x2010 > upper_bound)

outliers_2010 <- data[outlier_flag, c("Country Name", "Country Code", "2010")]
outliers_2010 <- outliers_2010[order(-outliers_2010$`2010`), ]

cat("\n2010 outlier thresholds:\n")
cat("Q1:", Q1, "\n")
cat("Q3:", Q3, "\n")
cat("IQR:", IQR_value, "\n")
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")
cat("Number of outliers:", sum(outlier_flag), "\n")
print(outliers_2010)

write.csv(outliers_2010,
          file.path(output_dir, "outliers_2010.csv"),
          row.names = FALSE)

# 7. Median imputation ----------------------------------------
# Median is used because fertility data can be skewed by very high values.
for (col in year_cols) {
  median_value <- median(data[[col]], na.rm = TRUE)
  data[[col]][is.na(data[[col]])] <- median_value
}

cat("\nMissing values after median imputation:\n")
print(colSums(is.na(data)))

# 8. Keep the original 2010 outlier flag after imputation -----
data$outlier_2010 <- as.integer(outlier_flag)

# 9. Min-max normalization of 2010 fertility ------------------
min_2010 <- min(data$`2010`, na.rm = TRUE)
max_2010 <- max(data$`2010`, na.rm = TRUE)

data$fertility_2010_normalized <-
  (data$`2010` - min_2010) / (max_2010 - min_2010)

# 10. Create a categorical fertility group --------------------
data$fertility_group <- cut(
  data$`2010`,
  breaks = c(-Inf, 2, 3, Inf),
  labels = c("Low (<2)", "Medium (2-3)", "High (>3)")
)

data$fertility_group <- factor(data$fertility_group)

# 11. Encode categorical variables ----------------------------
# Factor encoding for Country Code (for data-processing practice).
data$country_code_encoded <- as.integer(factor(data$`Country Code`))

# One-hot encoding for fertility_group.
encoded_groups <- model.matrix(~ fertility_group - 1, data = data)
encoded_groups <- as.data.frame(encoded_groups)

write.csv(encoded_groups,
          file.path(output_dir, "fertility_group_one_hot_encoding.csv"),
          row.names = FALSE)

# 12. Descriptive statistics ----------------------------------
summary_2010 <- data.frame(
  Statistic = c("Count", "Mean", "Median", "Standard Deviation",
                "Minimum", "Maximum"),
  Value = c(
    length(data$`2010`),
    mean(data$`2010`),
    median(data$`2010`),
    sd(data$`2010`),
    min(data$`2010`),
    max(data$`2010`)
  )
)

print(summary_2010)
write.csv(summary_2010,
          file.path(output_dir, "summary_statistics_2010_after_cleaning.csv"),
          row.names = FALSE)

# 13. Correlation analysis ------------------------------------
correlation_matrix <- cor(data[, year_cols], use = "complete.obs")
cat("\nCorrelation matrix for 2000-2011:\n")
print(round(correlation_matrix, 3))
write.csv(correlation_matrix,
          file.path(output_dir, "correlation_matrix.csv"))

# 14. Initial country-level insights --------------------------
ranked <- data[, c("Country Name", "Country Code", "2010",
                   "fertility_group", "fertility_2010_normalized")]
ranked <- ranked[order(-ranked$`2010`), ]

cat("\nTop 10 countries by 2010 fertility rate:\n")
print(head(ranked, 10))
write.csv(head(ranked, 10),
          file.path(output_dir, "top_10_2010.csv"),
          row.names = FALSE)

cat("\nBottom 10 countries by 2010 fertility rate:\n")
bottom10 <- ranked[order(ranked$`2010`), ]
bottom10 <- head(bottom10, 10)
print(bottom10)
write.csv(bottom10,
          file.path(output_dir, "bottom_10_2010.csv"),
          row.names = FALSE)

# 15. Yearly mean trend ---------------------------------------
yearly_means <- data.frame(
  Year = 2000:2011,
  Mean_Fertility = sapply(data[, year_cols], mean)
)
print(yearly_means)
write.csv(yearly_means,
          file.path(output_dir, "yearly_mean_trend.csv"),
          row.names = FALSE)

# 16. Visualizations ------------------------------------------
png(file.path(output_dir, "01_missing_values.png"), width = 1000, height = 550)
missing_years <- sapply(data_raw[, year_cols], function(x) sum(is.na(x)))
barplot(missing_years,
        main = "Missing Values by Year (2000-2011)",
        xlab = "Year", ylab = "Number of Missing Values")
dev.off()

png(file.path(output_dir, "02_2010_distribution.png"), width = 900, height = 550)
hist(observed_2010,
     breaks = 20,
     main = "Distribution of Fertility Rate in 2010",
     xlab = "Births per woman")
abline(v = median(observed_2010), lty = 2)
dev.off()

png(file.path(output_dir, "03_2010_boxplot.png"), width = 700, height = 550)
boxplot(observed_2010,
        main = "2010 Fertility Rate - Outlier Detection",
        ylab = "Births per woman")
dev.off()

png(file.path(output_dir, "04_top10_2010.png"), width = 1000, height = 650)
top10 <- head(ranked, 10)
barplot(rev(top10$`2010`),
        names.arg = rev(top10$`Country Name`),
        horiz = TRUE,
        las = 1,
        main = "Top 10 Countries by Fertility Rate, 2010",
        xlab = "Births per woman")
dev.off()

png(file.path(output_dir, "05_trend_2000_2011.png"), width = 950, height = 550)
plot(yearly_means$Year, yearly_means$Mean_Fertility,
     type = "o",
     main = "Average Fertility Rate Trend (2000-2011)",
     xlab = "Year", ylab = "Average births per woman")
grid()
dev.off()

png(file.path(output_dir, "06_bottom10_2010.png"), width = 1000, height = 650)
barplot(rev(bottom10$`2010`),
        names.arg = rev(bottom10$`Country Name`),
        horiz = TRUE,
        las = 1,
        main = "10 Lowest Fertility Rates, 2010",
        xlab = "Births per woman")
dev.off()

# 17. Save cleaned dataset ------------------------------------
write.csv(data,
          "world_bank_fertility_cleaned.csv",
          row.names = FALSE)

cat("\nAnalysis complete. Cleaned dataset and outputs have been saved.\n")
