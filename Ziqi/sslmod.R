# SSL model
# removes all objects and functions from the global environment
rm(list = ls())

# load packages
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(randomForest)

# import data
dta <- read.csv("ssldat.csv")


# distribution of elapsed_workdays
dta %>% ggplot(aes(elapsed_workdays)) + geom_histogram()
summary(dta$elapsed_workdays)

attach(dta)

## Classification tree
#create a binary outcome
over21 <- ifelse(elapsed_workdays>21,">21 days","<=21 days")
dta <- data.frame(dta,over21)

# create a formulat to use
dtree_formula <- as.formula("over21~.-elapsed_workdays")

# Fit the decision tree model
dtree_fit <- train(dtree_formula, data = dta, method = "rpart",
                   trControl = trainControl(method = 'cv',number = 10))


# Results of the cross-validation
dtree_fit$resample[,c(1,3)]
#mean(dtree_fit$resample$Accuracy)

# Visualize the decision tree 
rpart.plot(dtree_fit$finalModel)

# Extracting variable importance
dtree_VarImp <- varImp(dtree_fit)
dtree_VarImp <- dtree_VarImp$importance
dtree_VarImp$variables <- (rownames(dtree_VarImp))
dtree_VarImp <- dtree_VarImp[order(dtree_VarImp$Overall, decreasing = TRUE), ]

# Visualizing variable importance
ggplot(dtree_VarImp, aes(x = reorder(variables, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = 'steelblue') + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid =element_blank(),plot.title = element_text(hjust = 0.5)) +
  labs(title="Variables Importance - SSL Model",
       x ="Variables", y = "Variable Importance")

