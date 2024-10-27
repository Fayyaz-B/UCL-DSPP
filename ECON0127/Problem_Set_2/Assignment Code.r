#get working directory
getwd()

# Set working directory
setwd("/Users/faze/Documents/MSc DSPP/Modules/ECON0127/Term 1/PS/Week 4")

# Check if it successfully changed
getwd()

#Load necessary Libraries
library(tidyverse)

# Clear environment
rm(list=ls())

# Load in our data
data <- read_csv("ref2.csv")

# Check the data
head(data)
str(data)

## Task Set 1 

# To fit out logistic regression model, we need to convert our dependent variable to a quantitative variable
# We will convert the variable supp2 to a binary variable
data <- data %>% 
    mutate(supp2_binary = ifelse(supp2 >= 1, 1, 0))


# Fit a logistic regression model to predict if respondents took any actions to support refugees. 
# Use hinc2, female, age and college as predictors.
mod_1 <- glm(supp2_binary ~ hinc2 + female + age + college, data = data, family = binomial(link = "logit"))
summary(mod_1)

data$yhat <- ifelse (
    predict(mod_1, newdata = data, type = 'response') > 0.5, TRUE, FALSE
)

table(data$supp2_binary, data$yhat)

# Compute the classification accuracy
sum(data$yhat == data$supp2_binary, na.rm = TRUE) / sum(!is.na(data$yhat) & !is.na(data$supp2_binary))

# Sensitivity 
sum(data$yhat == TRUE & data$supp2_binary == 1, na.rm = TRUE) /
               sum(data$supp2_binary == 1, na.rm = TRUE)

# Specificity
sum(data$yhat == FALSE & data$supp2_binary == 0, na.rm = TRUE) /
               sum(data$supp2_binary == 0, na.rm = TRUE)


library(pROC)

# Use simulation to find the cutoff that maximises the classification accuracy
cutoffs <- seq(0, 1, by = 0.01)
accs <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
    data$yhat <- ifelse (
        predict(mod_1, newdata = data, type = 'response') > cutoffs[i], TRUE, FALSE
    )
    accs[i] <- sum(data$yhat == data$supp2_binary, na.rm = TRUE) / sum(!is.na(data$yhat) & !is.na(data$supp2_binary)
    )
}

cutoffs[which.max(accs)]


## Task Set 2 

#a 

# Import library to run LDA and QDA models
library(MASS)

# Use LDA to predict if respondents took any actions to support refugees
(mod_2 <- lda(supp2_binary ~ hinc2 + female + age + college, data = data))

# Compute the AUC for the LDA model
data$phat <- predict(mod_2, newdata = data)$posterior[, "1"]
m2_roc <- roc(supp2_binary ~ phat, data = data)

auc(m2_roc)

# Use QDA predict if respondents took any actions to support refugees
(mod_3 <- qda(supp2_binary ~ hinc2 + female +age + college, data = data))

data$qhat <- predict(mod_3, newdata = data)$posterior[, "1"]
m3_roc <- roc(supp2_binary ~ qhat, data = data)

auc(m3_roc)

# According to the Area Under Curve (AUC) values, the LDA model is better than the QDA and logistic regression models.

#b 

# Devise a simple procedure to assess which of the four variables is the least important for the predicitive performnce of the LDA model

# Sequential Variable Removal to Assess Variable Importance
# Description:
# This procedure involves removing each variable from the model one at a time,
# refitting the LDA and QDA models, and calculating the AUC. By comparing the 
# AUC with each variable removed, we can identify which variable has the least 
# impact on model performance (i.e., the least important variable). The variable 
# with the smallest AUC drop is considered the least important.


# Helper function to calculate AUC for a given formula and model type (LDA or QDA)
calculate_auc <- function(formula, data, model_type = "lda") {
  # Fit the specified model type
  if (model_type == "lda") {
    model <- lda(formula, data = data)
  } else if (model_type == "qda") {
    model <- qda(formula, data = data)
  }
  
  # Predict probabilities for the positive class ("1" in supp2_binary)
  data$phat <- predict(model, newdata = data)$posterior[, "1"]
  
  # Calculate and return AUC
  roc_obj <- roc(data$supp2_binary, data$phat)
  auc(roc_obj)
}

# Step 1: Calculate baseline AUC with all variables included for LDA and QDA
baseline_lda_auc <- calculate_auc(supp2_binary ~ hinc2 + female + age + college, data, model_type = "lda")
baseline_qda_auc <- calculate_auc(supp2_binary ~ hinc2 + female + age + college, data, model_type = "qda")

# Step 2: Initialize a data frame to store results for each variable removal
results <- data.frame(
  Variable = c("hinc2", "female", "age", "college"),  # List of variables to assess
  LDA_AUC = NA,  # Placeholder for LDA AUC without each variable
  QDA_AUC = NA   # Placeholder for QDA AUC without each variable
)

# Step 3: Loop through each variable, removing it and calculating AUC for both LDA and QDA
for (var in results$Variable) {
  # Define the formula without the current variable
  formula_lda <- as.formula(paste("supp2_binary ~", paste(setdiff(c("hinc2", "female", "age", "college"), var), collapse = " + ")))
  
  # Calculate AUC for the LDA model without the current variable
  results$LDA_AUC[results$Variable == var] <- calculate_auc(formula_lda, data, model_type = "lda")
  
  # Calculate AUC for the QDA model without the current variable
  results$QDA_AUC[results$Variable == var] <- calculate_auc(formula_lda, data, model_type = "qda")
}

# Step 4: Calculate the drop in AUC for each variable by comparing to the baseline AUC
results$LDA_AUC_Drop <- baseline_lda_auc - results$LDA_AUC
results$QDA_AUC_Drop <- baseline_qda_auc - results$QDA_AUC

# Display the results, including AUC drop for each variable removal
print(results)

# Step 5: Identify the least important variable for both LDA and QDA
# The variable with the smallest drop in AUC is considered the least important.
cat("Least important variable for LDA:", results$Variable[which.min(results$LDA_AUC_Drop)], "\n")
cat("Least important variable for QDA:", results$Variable[which.min(results$QDA_AUC_Drop)], "\n")


# c 

# Construct a variable that takes value "no” if a respondent provided no support to refugees in the past,
# “donated” if a respondent donated money or goods in the past and “direct” if a respondent hosted refugees or provided other direct direct
# support

data <- data %>%
    mutate(supp_actions = ifelse(supp_money == 0 & supp_goods == 0, "no",
                          ifelse(supp_money == 1 | supp_goods == 1, "donated",
                          ifelse(supp_inv == 1 | supp_dir == 1, "direct", NA))))

# d 

# Use LDA to predict which actions respondents took to support refugees
# use hinc2, female, age and college as predictors

(mod_4 <- lda(supp_actions ~ hinc2 + female + age + college, data = data ))

# Use QDA to predict which actions respondents took to support refugees

(mod_5 <- qda(supp_actions ~ hinc2 + female + age + college, data = data))

# Compute the accuracy of the LDA model
data$lhat <- ifelse(
    predict(mod_4, newdata = data, type = 'response')>0.5, TRUE, FALSE
)

table(data$supp_actions, data$lhat)

# Accuracy
sum(data$lhat == data$supp_actions, na.rm = TRUE) / sum(!is.na(data$lhat) & !is.na(data$supp_actions))

# Compute the accuracy of the QDA model
data$qhat <- ifelse(
    predict(mod_5, newdata = data, type = 'response')>0.5, TRUE, FALSE
)

table(data$supp_actions, data$qhat)

# Accuracy
sum(data$qhat == data$supp_actions, na.rm = TRUE) / sum(!is.na(data$qhat) & !is.na(data$supp_actions))

# e 


# f 
# LDA does indeed assume that all predictors are distributed multivarite normal. However, LDA appears to be robust enough to handle some violations of this assumption, 
# especially when the sample size is large. In practice, LDA can still perform well even when the normality assumption is not strictly met, as long as the data is not too skewed or heavy-tailed. 
# we could run logistic regression alongside LDA and compare their performance using cross-validation. If neither approach performs well due to severe non-normality or non-linear relationships, 
# we might also consider non-parametric methods like decision trees or random forests, which are more flexible with data assumptions.

## Task Set 3 
