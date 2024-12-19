#MODEL DEPLOYMENT
#Loading Necessary Libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(randomForest)
library(reshape2)
library(GGally)
library(survival)
library(randomForestSRC)
library(gbm)



#Duplicating the dataset
breast_Cancer_Dataa <- read_csv("C:/Users/2372550/OneDrive - University of Wolverhampton/Desktop/Breast_Cancer.csv")

#Replace space with underscore in certain columns
colnames(breast_Cancer_Dataa) <- gsub(" ", "_", colnames(breast_Cancer_Dataa))
# Check the updated column names
colnames(breast_Cancer_Dataa)

# Identify character columns
character_cols <- sapply(breast_Cancer_Dataa, is.character)
names(breast_Cancer_Dataa)[character_cols]

# Convert character columns to factors
breast_Cancer_Dataa <- data.frame(lapply(breast_Cancer_Dataa, function(x) {
  if (is.character(x)) as.factor(x) else x
}))

# Check for factor (categorical) columns to determine if it is Ordinal or Nominal 
categorical_columns <- sapply(breast_Cancer_Dataa, is.factor)
# Display unique levels for each categorical variable
lapply(breast_Cancer_Dataa[, categorical_columns], unique)

# Convert necessary columns to factors
breast_Cancer_Dataa$`sixth_Stage` <- as.factor(breast_Cancer_Dataa$`X6th_Stage`)
Categorical_cols <- c("Race", "Marital_Status", "T_Stage", "N_Stage", "sixth_Stage"
                      , "differentiate", "Grade", "A_Stage", 
                      "Estrogen_Status", "Progesterone_Status", "Status")

breast_Cancer_Dataa[Categorical_cols] <- lapply(breast_Cancer_Dataa[Categorical_cols], as.factor)

# 3. Check Data Types
str(breast_Cancer_Dataa)

# 4. Check and Handle Outliers
# List of continuous columns
continuous_cols <- c("Age", "Tumor_Size", "Regional_Node_Examined", "Reginol_Node_Positive", "Survival_Months")

# Handle outliers by capping extreme values
cap_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  caps <- c(qnt[1] - 1.5 * IQR(x), qnt[2] + 1.5 * IQR(x))
  x[x < caps[1]] <- caps[1]
  x[x > caps[2]] <- caps[2]
  return(x)
}

breast_Cancer_Dataa[continuous_cols] <- lapply(breast_Cancer_Dataa[continuous_cols], cap_outliers)

# Scale continuous variables
preProc <- preProcess(breast_Cancer_Dataa[, continuous_cols], method = c("center", "scale"))
breast_Cancer_Dataa[, continuous_cols] <- predict(preProc, breast_Cancer_Dataa[, continuous_cols])

#TRAINING MY DATA
# Load survival package
library(survival)
library(caret)

# Set seed for reproducibility
set.seed(42)

# Split the dataset into training (70%) and testing (30%) sets
train_index <- createDataPartition(breast_Cancer_Dataa$Survival_Months, p = 0.7, list = FALSE)
train_data <- breast_Cancer_Dataa[train_index, ]
test_data <- breast_Cancer_Dataa[-train_index, ]

# Verify the dimensions of the splits
cat("Training Data Dimensions:", dim(train_data), "\n")
cat("Testing Data Dimensions:", dim(test_data), "\n")

# Convert Status to numeric (1 = Event, 0 = Censored)
train_data$Status <- ifelse(train_data$Status == "Dead", 1, 0)
test_data$Status <- ifelse(test_data$Status == "Dead", 1, 0)

# Check the distribution of Status
table(train_data$Status)
table(test_data$Status)


library(survival)

# Fit Cox Proportional Hazards model
cox_model <- coxph(Surv(Survival_Months, Status) ~ ., data = train_data)

# Summarize the Cox model
summary(cox_model)

# Calculate Concordance Index for the Cox model on training data
cox_preds <- predict(cox_model, newdata = test_data, type = "risk")
cox_c_index <- survConcordance(Surv(Survival_Months, Status) ~ cox_preds, data = test_data)$concordance
cat("C-index for Cox Proportional Hazards Model:", cox_c_index, "\n")

#FIT FOR RSF
library(randomForestSRC)
rsf_model <- rfsrc(Surv(Survival_Months, Status) ~ ., data = train_data, ntree = 100)
print(rsf_model)

# Calculate the C-index
cox_c_index <- concordance(Surv(Survival_Months, Status) ~ rsf_preds_summary, data = test_data)$concordance
# Print the C-index
print(paste("C-index for RSF:", C_index_rsf))


# Compare results
cat("C-index for Cox:", cox_c_index, "\n")
cat("C-index for Random Survival ForestModel :", C_index_rsf, "\n")



#GMB Model
# Fit GBM Model
gbm_model <- gbm(
  formula = Surv(Survival_Months, Status) ~ .,
  data = train_data,
  distribution = "coxph",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  cv.folds = 5,
  verbose = TRUE
)

# Determine optimal number of trees
optimal_trees <- gbm.perf(gbm_model, method = "cv")
cat("Optimal number of trees:", optimal_trees, "\n")

# Predict on test data
gbm_preds <- predict(gbm_model, newdata = test_data, n.trees = optimal_trees, type = "response")

# Calculate C-index
gbm_c_index <- concordance(Surv(Survival_Months, Status) ~ gbm_preds, data = test_data)$concordance
cat("C-index for GBM:", gbm_c_index, "\n")


cat("C-index Comparison:\n")
cat("C-index for Cox:", cox_c_index, "\n")
cat("C-index for RSF:", C_index_rsf, "\n")
cat("C-index for Gradient Boosting Model:", gbm_c_index, "\n")


#Model Comparism
library(pec)        # For Brier Score and Calibration
library(riskRegression)  # For model comparison
library(survival)   # For Cox model
library(randomForestSRC) # For RSF
library(gbm)        # For GBM
library(ggplot2)
library(pec)

# Fit the CoxPH model
cox_model <- coxph(Surv(Survival_Months, Status) ~ ., data = train_data, x = TRUE)

# Fit the Random Survival Forest model
rsf_model <- rfsrc(Surv(Survival_Months, Status) ~ ., data = train_data, ntree = 100)

# Fit the GBM model
gbm_model <- gbm(
  formula = Surv(Survival_Months, Status) ~ .,
  data = train_data,
  distribution = "coxph",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  cv.folds = 5,
  verbose = FALSE
)
optimal_trees <- gbm.perf(gbm_model, method = "cv")


# Predict survival probabilities
cox_pred <- predictSurvProb(cox_model, newdata = test_data, times = 12)
rsf_pred <- predictSurvProb(rsf_model, newdata = test_data, times = 12)
gbm_pred <- predict(gbm_model, newdata = test_data, n.trees = optimal_trees, type = "response")

# Brier Score Comparison
brier_score <- pec(
  object = list("Cox" = cox_model, "RSF" = rsf_model),
  formula = Surv(Survival_Months, Status) ~ .,
  data = test_data,
  times = seq(12, 60, by = 12)
)
print(brier_score)



# Brier Score Comparison
brier_score <- pec(
  object = list("Cox" = cox_model, "RSF" = rsf_model),
  formula = Surv(Survival_Months, Status) ~ .,   # Survival formula
  data = test_data,                             # Full test data
  times = seq(12, 60, by = 12),                 # Time points for evaluation
  exact = TRUE                                  # Avoid approximation issues
)

# Print Brier Score results
print(brier_score)

predictor_vars <- setdiff(names(train_data), c("Survival_Months", "Status"))
predictor_formula <- as.formula(
  paste("Surv(Survival_Months, Status) ~", paste(predictor_vars, collapse = " + "))
)


# Plot the Brier Score curves
plot(brier_score, xlab = "Time (Months)", ylab = "Brier Score", 
     main = "Brier Score Comparison")

# AIC for CoxPH
aic_cox <- AIC(cox_model)
cat("AIC for Cox Model:", aic_cox, "\n")

# Calibration Plot for Cox and RSF
cal_plot <- autoplot(pec::calPlot(
  object = list("Cox" = cox_model, "RSF" = rsf_model),
  formula = Surv(Survival_Months, Status) ~ .,
  data = test_data,
  times = 12
))
print(cal_plot)

# Summary of Results
cat("Brier Score Comparison:\n")
print(brier_score)

cat("AIC for Cox Proportional Hazards Model:", aic_cox, "\n")
