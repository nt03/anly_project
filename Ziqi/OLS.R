# OLS

# load packages
library(car)

# import data
dat <- read.csv("cleaned_merged_data5.csv")
attach(dat)

# fit the model
ols <- lm(elapsed_workdays~factor(alias)+factor(permit_cap_status)+factor(GroupName)
          +factor(RC_ReviewStatus)+factor(RC_HowAssigned)+factor(job_class)+Ward+green_floor_area
          +factor(review_group)+factor(use_type)+factor(building_construction_type)+PRICE+factor(QUALIFIED)
          +factor(EYB)+SALE_NUM+LIVING_GBA+USECODE+factor(use_change)+factor(num_units_change)+factor(gfa_change),
          data = dat)

ols_output <- broom::tidy(ols)

# evaluation
# VIF-- multicolinearity
vif(ols)
# ?RC_ReviewStatus, RC_HowAssigned, EYB -- highly correlated

# prediction
preds <- mean(predict(ols))

## MSE
set.seed(1)
train=sample(35834,35834/2)
mean((elapsed_workdays-predict(ols,dat))[-train]^2)

