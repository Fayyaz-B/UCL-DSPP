#getwd() 
getwd()

# Set working directory
setwd("/Users/faze/Documents/MSc DSPP/Modules/ECON0127/Term 1/PS/Week 4")

# Check if it successfully changed
getwd()

#Load necessary Libraries
library(tidyverse)
library(janitor)

# Clear environment
rm(list=ls())

# Load in our data
data <- read_csv("ref2.csv") %>%
    clean_names() 

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

