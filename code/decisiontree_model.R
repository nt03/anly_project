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
dat <- read.csv("../data/final_permit_encode.csv") %>%
  select(-pdox_b1_id, -over_90, -over_150)

summary(dat$elapsed_workdays)

attach(dat)

## Classification tree
#create a binary outcome
over150 <- ifelse(elapsed_workdays>150,">150 days","<=150 days")
dat <- data.frame(dat,over150)

# create a formulat to use
dtree_formula <- as.formula("over150~.-elapsed_workdays")

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
dtree_VarImp <- dtree_VarImp[order(dtree_VarImp$Overall, decreasing = TRUE), ][1:8,]

# Visualizing variable importance
ggplot(dtree_VarImp, aes(x = reorder(variables, Overall), y = Overall)) +
  geom_bar(stat = 'identity',fill = 'darkblue') + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid =element_blank(),plot.title = element_text(face = 'bold',size= 16,hjust = 0.5),
        axis.text = element_text(face = 'bold',size = 11))+
  labs(title="Variables Importance",
       x ="", y = "")
