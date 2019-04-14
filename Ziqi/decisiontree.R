# Tree
library(car)
library(tree)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)

# import data
dat <- read.csv("cleaned_merged_data5.csv")  %>%
  select(-TaskName,-GroupName,-ssl)


attach(dat)

## Classification tree
#create a binary outcome
over30 <- ifelse(elapsed_workdays>30,">30 days","<=30 days")
dat <- data.frame(dat,over30)

# fit decision tree model
# split train and test set
set.seed(1)
train = sample(35834,35834/2)
test = dat[-train,]
over30_test = over30[-train]

dtree <- tree(over30~factor(alias)+factor(permit_cap_status)+factor(Project_Status)+ ReviewCycle +DC.Water.Review+DOEE+Electrical.Review+Energy.Review
              +Fire.Review+Green.Review+Mechanical.Review+Plumbing.Review+Structural.Review+Zoning.Review
              +factor(RC_ReviewStatus)+factor(RC_HowAssigned)+factor(job_class)+factor(AGENCY)+Ward+green_floor_area
              +factor(review_group)+factor(use_type)+factor(permit_type)+factor(building_construction_type)+PRICE+factor(QUALIFIED)
              +SALE_NUM+LIVING_GBA+USECODE+factor(use_change)+factor(num_units_change)+factor(gfa_change)+EYB+LANDAREA,
              data = dat,subset = train)

# predictions
pred <- predict(dtree,test,type = "class")
table(pred,over30_test)
# score
(16216+447)/(35834/2)

summary(dtree)

# create a formula to use
dtree_formula <- as.formula("over30~factor(alias)+factor(permit_cap_status)+factor(Project_Status)+ ReviewCycle +DC.Water.Review+DOEE+Electrical.Review+Energy.Review
              +Fire.Review+Green.Review+Mechanical.Review+Plumbing.Review+Structural.Review+Zoning.Review
                            +factor(RC_ReviewStatus)+factor(RC_HowAssigned)+factor(job_class)+factor(AGENCY)+Ward+green_floor_area
                            +factor(review_group)+factor(use_type)+factor(permit_type)+factor(building_construction_type)+PRICE+factor(QUALIFIED)
                            +SALE_NUM+LIVING_GBA+USECODE+factor(use_change)+factor(num_units_change)+factor(gfa_change)+EYB+LANDAREA")

# Fit the decision tree model
dtree_fit <- train(dtree_formula, data = dat, method = "rpart")
dtree_fit

# Cross-validation - 5 fold
trCtrl <- trainControl(method = "cv", number = 5)
dtree_fit_cv <- train(dtree_formula, data=dat,
                      method = "rpart", trControl = trCtrl)

# Results of the cross-validation
dtree_fit_cv$resample[c(1,3)]

# Visualize the decision tree 
rpart.plot(dtree_fit_cv$finalModel)

# Extracting variable importance
dtree_VarImp <- varImp(dtree_fit)
dtree_VarImp <- dtree_VarImp$importance
dtree_VarImp$variables <- (rownames(dtree_VarImp))
dtree_VarImp <- dtree_VarImp[order(dtree_VarImp$Overall, decreasing = TRUE), ]

# Visualizing variable importance
ggplot(dtree_VarImp, aes(x = reorder(variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_bw() +
  labs(title="Variables Importance - Classification Tree",
       x ="Variables", y = "Variable Importance")

# prune
set.seed(3)
# CV
cv <- cv.tree(dtree,FUN = prune.misclass)
names(cv)
cv
# when size = 6 and 2, devs are relatively small with 1332 cv errors
#-- best=2--DC Water Review and Review Cycle

## Random Forest
rf_fit <- train(dtree_formula, data = dat, method = "rf",  ntree = 1000, 
                trControl =  trainControl(method = "cv", number = 5))  

# Results of the cross-validation
rf_fit$resample[ , c(1,3)]

# Variable importance
varImp(rf_fit)
# Visualize the decision tree 
rpart.plot(dtree_fit$finalModel)


##=================================================================
## Regression tree
rtree <- tree(elapsed_workdays~factor(alias)+factor(permit_cap_status)+DC.Water.Review+DOEE+Electrical.Review+Energy.Review
              +Fire.Review+Green.Review+Mechanical.Review+Plumbing.Review+Structural.Review+Zoning.Review
              +factor(RC_ReviewStatus)+factor(RC_HowAssigned)+factor(job_class)+Ward+green_floor_area
              +factor(review_group)+factor(use_type)+factor(building_construction_type)+PRICE+factor(QUALIFIED)
              +SALE_NUM+LIVING_GBA+USECODE+factor(use_change)+factor(num_units_change)+factor(gfa_change)+EYB,
              data = dat,subset = train)
summary(rtree)

# plot
plot(rtree)
text(rtree,pretty = 0)

# prune
cv_rtree <- cv.tree(rtree)
plot(cv_rtree$size,cv_rtree$dev,type = 'b')
# best=6, no need to prune

# predict
yhat <- predict(rtree,newdata = test)
days_test <- dat[-train,"elapsed_workdays"]
plot(yhat,days_test)
abline(0,1)
# MSE
mean((yhat-days_test)^2)
# 159.6934 -- even worse than ols