# WEEK 4 - COMPREHENSIVE DATA ANALYSIS REPORTING AND PRESENTATION
# Reproducible R workflow integrating Week 1, Week 2 and Week 3.
# Run from the project root in RStudio: source("02_R_Scripts/Week4_Complete_Analysis.R")
# Required inputs: 01_Datasets/world_bank_fertility.csv and insurance.csv
# Week 2 data is supplied by ggplot2::economics.

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(42)
required <- c("ggplot2", "dplyr", "scales")
missing <- required[!vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(scales)})
folders <- c("03_Cleaned_Data", "04_Visualizations/Week1_Fertility", "04_Visualizations/Week2_Economics", "04_Visualizations/Week3_Insurance", "05_Statistical_Results", "06_Model_Diagnostics", "07_Execution_Logs")
for (d in folders) dir.create(d, recursive=TRUE, showWarnings=FALSE)

# WEEK 1: Fertility data cleaning
f <- read.csv("01_Datasets/world_bank_fertility.csv", check.names=FALSE)
years <- as.character(2000:2011)
stopifnot(all(c("Country Name", "Country Code", years) %in% names(f)))
f <- f[, c("Country Name", "Country Code", years)]
for (v in years) f[[v]] <- as.numeric(f[[v]])
missing_tbl <- data.frame(variable=names(f), missing_count=sapply(f, function(x) sum(is.na(x))))
write.csv(missing_tbl, "05_Statistical_Results/week1_missing_values_R.csv", row.names=FALSE)
x <- f[["2010"]]; q1 <- quantile(x, .25, na.rm=TRUE); q3 <- quantile(x, .75, na.rm=TRUE); iqr <- IQR(x, na.rm=TRUE)
flag <- !is.na(x) & (x < q1-1.5*iqr | x > q3+1.5*iqr)
out <- f[flag, c("Country Name", "Country Code", "2010")]
write.csv(out, "05_Statistical_Results/week1_outliers_R.csv", row.names=FALSE)
for (v in years) f[[v]][is.na(f[[v]])] <- median(f[[v]], na.rm=TRUE)
f$outlier_2010 <- as.integer(flag)
f$fertility_2010_normalized <- (f[["2010"]]-min(f[["2010"]]))/(max(f[["2010"]])-min(f[["2010"]]))
f$fertility_group <- cut(f[["2010"]], breaks=c(-Inf,2,3,Inf), labels=c("Low (<2)","Medium (2-3)","High (>3)"))
f$country_code_encoded <- as.integer(factor(f$`Country Code`))
write.csv(f, "03_Cleaned_Data/world_bank_fertility_cleaned_R.csv", row.names=FALSE)
trend <- data.frame(year=2000:2011, mean_fertility=sapply(f[,years], mean))
write.csv(trend, "05_Statistical_Results/week1_yearly_mean_R.csv", row.names=FALSE)
ggsave("04_Visualizations/Week1_Fertility/01_missing_values_R.png", ggplot(missing_tbl, aes(x=variable,y=missing_count))+geom_col()+labs(title="Missing Values by Year",x="Variable",y="Missing records")+theme_minimal(), width=9,height=5,dpi=300)
ggsave("04_Visualizations/Week1_Fertility/02_fertility_distribution_R.png", ggplot(f,aes(x=`2010`))+geom_histogram(bins=22,color="white",fill="#2878B5")+labs(title="Fertility Rate Distribution, 2010",x="Births per woman",y="Countries")+theme_minimal(),width=9,height=5,dpi=300)
ggsave("04_Visualizations/Week1_Fertility/03_fertility_trend_R.png", ggplot(trend,aes(year,mean_fertility))+geom_line()+geom_point()+labs(title="Mean Fertility Rate Trend, 2000-2011",x="Year",y="Mean births per woman")+theme_minimal(),width=9,height=5,dpi=300)

# WEEK 2: Economics visualization using the ggplot2 built-in dataset
data("economics", package="ggplot2")
econ <- economics %>% mutate(year=as.integer(format(date,"%Y")), decade=floor(year/10)*10)
econ_stats <- data.frame(observations=nrow(econ), start_date=min(econ$date), end_date=max(econ$date), mean_unemployment=mean(econ$unemploy), min_unemployment=min(econ$unemploy), max_unemployment=max(econ$unemploy), pce_unemployment_correlation=cor(econ$pce,econ$unemploy))
write.csv(econ_stats,"05_Statistical_Results/week2_economics_summary_R.csv",row.names=FALSE)
ggsave("04_Visualizations/Week2_Economics/01_unemployment_trend_R.png",ggplot(econ,aes(date,unemploy))+geom_line()+labs(title="Unemployment Trend",x="Date",y="Unemployed (thousands)")+theme_minimal(),width=10,height=5.5,dpi=300)
ggsave("04_Visualizations/Week2_Economics/02_pce_trend_R.png",ggplot(econ,aes(date,pce))+geom_line()+labs(title="Personal Consumption Expenditure Trend",x="Date",y="PCE (billions USD)")+theme_minimal(),width=10,height=5.5,dpi=300)
ggsave("04_Visualizations/Week2_Economics/03_unemployment_histogram_R.png",ggplot(econ,aes(unemploy))+geom_histogram(bins=28,color="white",fill="#2878B5")+labs(title="Monthly Unemployment Distribution",x="Unemployed (thousands)",y="Months")+theme_minimal(),width=9,height=5,dpi=300)
ggsave("04_Visualizations/Week2_Economics/04_pce_unemployment_R.png",ggplot(econ,aes(pce,unemploy))+geom_point(alpha=.45)+geom_smooth(method="lm",se=FALSE)+labs(title="PCE vs Unemployment",x="PCE (billions USD)",y="Unemployed (thousands)")+theme_minimal(),width=9,height=5,dpi=300)
dec <- econ %>% group_by(decade) %>% summarise(avg_unemploy=mean(unemploy),.groups="drop")
ggsave("04_Visualizations/Week2_Economics/05_unemployment_by_decade_R.png",ggplot(dec,aes(factor(decade),avg_unemploy))+geom_col(fill="#2878B5")+labs(title="Average Unemployment by Decade",x="Decade",y="Average unemployed (thousands)")+theme_minimal(),width=9,height=5,dpi=300)

# WEEK 3: Insurance charges analysis and predictive regression
ins <- read.csv("01_Datasets/insurance.csv", na.strings=c("","NA","N/A"))
expected <- c("age","sex","bmi","children","smoker","region","charges")
if (!all(expected %in% names(ins))) stop("insurance.csv does not have the expected Kaggle columns.")
ins <- ins[,expected]
for (v in c("sex","smoker","region")) ins[[v]] <- factor(tolower(trimws(ins[[v]])))
ins <- ins[!duplicated(ins),]
valid <- complete.cases(ins) & ins$age>=0 & ins$age<=120 & ins$bmi>0 & ins$bmi<=100 & ins$children>=0 & ins$children==floor(ins$children) & ins$charges>=0
ins <- droplevels(ins[valid,])
if (nrow(ins)<100) stop("Too few valid insurance rows remain.")
write.csv(ins,"03_Cleaned_Data/insurance_cleaned_R.csv",row.names=FALSE)
desc <- data.frame(variable=c("age","bmi","children","charges"),mean=sapply(ins[,c("age","bmi","children","charges")],mean),median=sapply(ins[,c("age","bmi","children","charges")],median),sd=sapply(ins[,c("age","bmi","children","charges")],sd))
write.csv(desc,"05_Statistical_Results/week3_descriptive_R.csv",row.names=FALSE)
cor_mat <- cor(ins[,c("age","bmi","children","charges")],method="spearman")
write.csv(cor_mat,"05_Statistical_Results/week3_spearman_R.csv")
smoker_test <- t.test(charges ~ smoker, data=ins)
age_test <- cor.test(ins$age,ins$charges,method="spearman",exact=FALSE)
tests <- data.frame(test=c("Welch t-test: charges by smoker","Spearman: age vs charges"),statistic=c(unname(smoker_test$statistic),unname(age_test$statistic)),p_value=c(smoker_test$p.value,age_test$p.value))
write.csv(tests,"05_Statistical_Results/week3_tests_R.csv",row.names=FALSE)
set.seed(42); idx <- sample(seq_len(nrow(ins)),size=floor(.8*nrow(ins)))
train <- ins[idx,]; test <- ins[-idx,]
fit <- lm(charges ~ age + bmi + children + sex + smoker + region,data=train)
pred <- predict(fit,newdata=test)
mae <- mean(abs(test$charges-pred)); rmse <- sqrt(mean((test$charges-pred)^2)); test_r2 <- 1-sum((test$charges-pred)^2)/sum((test$charges-mean(test$charges))^2)
model_metrics <- data.frame(metric=c("train_rows","test_rows","MAE_USD","RMSE_USD","test_R_squared","train_R_squared","adjusted_R_squared","F_statistic","F_p_value"),value=c(nrow(train),nrow(test),mae,rmse, test_r2,summary(fit)$r.squared,summary(fit)$adj.r.squared,unname(summary(fit)$fstatistic[1]),pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3],lower.tail=FALSE)))
write.csv(model_metrics,"05_Statistical_Results/week3_model_metrics_R.csv",row.names=FALSE)
write.csv(data.frame(actual=test$charges,predicted=as.numeric(pred),residual=test$charges-as.numeric(pred)),"06_Model_Diagnostics/test_predictions_R.csv",row.names=FALSE)
ggsave("04_Visualizations/Week3_Insurance/01_charges_distribution_R.png",ggplot(ins,aes(charges))+geom_histogram(bins=35,color="white",fill="#2878B5")+labs(title="Distribution of Medical Insurance Charges",x="Charges (USD)",y="Records")+theme_minimal(),width=9,height=5,dpi=300)
ggsave("04_Visualizations/Week3_Insurance/02_age_vs_charges_R.png",ggplot(ins,aes(age,charges))+geom_point(alpha=.5)+geom_smooth(method="lm")+labs(title="Age vs Medical Insurance Charges",x="Age",y="Charges (USD)")+theme_minimal(),width=9,height=5,dpi=300)
ggsave("04_Visualizations/Week3_Insurance/03_charges_by_smoker_R.png",ggplot(ins,aes(smoker,charges,fill=smoker))+geom_boxplot()+labs(title="Charges by Smoking Status",x="Smoker",y="Charges (USD)")+theme_minimal()+theme(legend.position="none"),width=8,height=5,dpi=300)
ggsave("04_Visualizations/Week3_Insurance/04_charges_by_region_R.png",ggplot(ins,aes(region,charges,fill=region))+geom_boxplot()+labs(title="Charges by Region",x="Region",y="Charges (USD)")+theme_minimal()+theme(legend.position="none"),width=9,height=5,dpi=300)
ggsave("06_Model_Diagnostics/01_actual_vs_predicted_R.png",ggplot(data.frame(actual=test$charges,predicted=as.numeric(pred)),aes(actual,predicted))+geom_point(alpha=.6)+geom_abline(linetype="dashed")+labs(title="Actual vs Predicted Charges",x="Actual charges (USD)",y="Predicted charges (USD)")+theme_minimal(),width=8,height=5,dpi=300)
ggsave("06_Model_Diagnostics/02_residuals_R.png",ggplot(data.frame(predicted=as.numeric(pred),residual=test$charges-as.numeric(pred)),aes(predicted,residual))+geom_point(alpha=.6)+geom_hline(yintercept=0,linetype="dashed")+labs(title="Residuals vs Predicted",x="Predicted charges (USD)",y="Residuals (USD)")+theme_minimal(),width=8,height=5,dpi=300)
writeLines(capture.output(sessionInfo()),"07_Execution_Logs/R_sessionInfo.txt")
cat("Week 4 workflow completed. Review outputs and model diagnostics before submission.\n")
