# Decision Tree

# removes all objects and functions from the global environment
rm(list = ls())

# load packages
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(randomForest)

# import data
dat <- read.csv("cleaned_encode2.csv")

# distribution of elapsed_workdays
dat %>% ggplot(aes(elapsed_workdays)) + geom_histogram() + xlim(0,30)

summary(dat$elapsed_workdays)

attach(dat)

## Classification tree
#create a binary outcome
over14 <- ifelse(elapsed_workdays>14,">14 days","<=14 days")
dat <- data.frame(dat,over14) %>% 
  select(-pdox_b1_id, -perm_id,-elapsed_workdays_7,-elapsed_workdays_14,-elapsed_workdays_21,-elapsed_workdays_28) %>%
  mutate(review_group=ifelse(review_group==0,"DepartmentReview","Supervisor"))


# create a formulat to use
dtree_formula <- as.formula("over14~.-elapsed_workdays")

# Fit the decision tree model
dtree_fit <- train(dtree_formula, data = dat, method = "rpart", 
                   trControl = trainControl(method = 'cv',number = 10))

# Results of the cross-validation
dtree_fit$resample[,c(1,3)]
mean(dtree_fit$resample$Accuracy)

# Visualize the decision tree 
rpart.plot(dtree_fit$finalModel)

# Extracting variable importance
dtree_VarImp <- varImp(dtree_fit)
dtree_VarImp <- dtree_VarImp$importance
dtree_VarImp$variables <- (rownames(dtree_VarImp))
dtree_VarImp <- dtree_VarImp[order(dtree_VarImp$Overall, decreasing = TRUE), ][1:13,]

# Visualizing variable importance
ggplot(dtree_VarImp, aes(x = reorder(variables, Overall), y = Overall),cex.lab=5) +
  geom_bar(stat = "identity", fill = 'steelblue') + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid =element_blank(),plot.title = element_text(hjust = 0.5)) +
  labs(title="Variables Importance - Permit Model",
       x ="Variables", y = "Variable Importance")

