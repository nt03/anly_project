## target array -- elapsed_workdays

library(tidyverse)
dat <- read_csv(file = 'merged_dcra_cama.csv', col_names = TRUE) 

# variables of interst
pid <- dat %>%
  group_by(perm_id) %>%
  summarise(n=n())
# duplicate rows arise

status <- dat %>%
  group_by(TaskStatus) %>%
  summarise(n=n())

agency <- dat %>%
  group_by(AGENCY) %>%
  summarise(n=n())

# assess duplicate rows
fil <- dat %>%
  filter(perm_id=='6014340')
# only AYB,EYB,LIVING_GBA different

dat <- dat %>%
  group_by(perm_id) %>%
  filter(row_number()== 1) # remove duplicates by keeping the first row for each unique perm_id
# some rows in original permit_review dataset are duplicated after merging

# according to DaaDicionary, it will be easier to interpert elapsed_workdays 
# if TaskStatus=Complete and exclude customer operations
dat <- dat %>%
  filter(TaskStatus=='Complete' & AGENCY != 'CUSTOMER')

write.csv(dat,'dat.csv')
#==============================
# if we set pdox id as unit of analysis
days_per_pdox <- dat %>%
  group_by(pdox_b1_id) %>%
  summarise(days = sum(elapsed_workdays))

