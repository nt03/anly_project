## target array -- elapsed_workdays

library(tidyverse)
dat <- read_csv(file = 'merged_dcra_cama.csv', col_names = TRUE)

# get a sense of types within variables of interest
pid <- dat %>% 
  group_by(perm_id) %>% 
  summarise(n = n())

status <- dat %>%
  group_by(TaskStatus) %>%
  summarise(n=n())

agency <- dat %>%
  group_by(AGENCY) %>%
  summarise(n=n())

# according to DaaDicionary, it will be easier to interpert elapsed_workdays 
# if TaskStatus=Complete and exclude customer operations
dat <- dat %>%
  filter(TaskStatus=='Complete' & AGENCY != 'CUSTOMER')

#==============================
# if we set perm_id as unit of analysis
days_per_permit <- dat %>%
  group_by(perm_id) %>%
  summarise(days = sum(elapsed_workdays))
