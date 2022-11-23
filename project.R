#getwd()
#setwd('/Users/apgillock/Dropbox/MIS 382N Advanced Machine Learning/Project/dataset_diabetes')
data <- read.csv('diabetic_data.csv')

head(data)
View(data)
dim(data) # 101766 x 50

library(tidyverse)

##############################################################################################

# deal with missing values
sum(is.na(data))
sum(is.null(data))

data %>% group_by(race) %>% count() # 2273 missing race
data %>% group_by(weight) %>% count() # 98569 missing weight
data %>% group_by(payer_code) %>% count() # 40256 missing payer_code
data %>% group_by(medical_specialty) %>% count() # 49949 missing medical_specialty

# get rid of '?'
data <- data %>% 
  select(-weight, -payer_code, -medical_specialty) # de-select irrelevant columns
  
data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% # remove unknown instances of diagnoses
  filter_all(any_vars(. %in% '?')) # check for ?s

data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% 
  filter_all(any_vars(. %in% '?')) %>% 
  filter(race != '?') # remove unknown instances of race

data <- data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% 
  filter(race != '?')

data %>% filter_all(any_vars(. %in% '?')) %>% count() # ensure no more missing values

dim(data) # after removing missing values, we are left with 98053 rows, 47 columns

##############################################################################################

# how much numeric data do we have?
data %>% select_if(is.numeric) %>% dim() # 98053 x 13
data %>% select_if(is.numeric) %>% head()

##############################################################################################

# select records relating to medication
names(data)
meds <- names(data)[22:38]

data %>% select(any_of(meds)) %>% count()

##############################################################################################

# let's start by considering a binary response variable
data %>% group_by(readmitted) %>% count()

data2 <- data %>% mutate(
  readmitted = case_when(
    readmitted == '>30' ~ 1,
    readmitted == '<30' ~ 1,
    readmitted == 'NO' ~ 0))

data2 %>% group_by(readmitted) %>% count()

# now we encode medication variables
data2 <- data2 %>% 
  mutate_at(
  vars(any_of(meds)),
  funs(case_when(
    . == 'Steady' ~ 1,
    . == 'Down' ~ 1,
    . == 'Up' ~ 1,
    . == 'No' ~ 0)
  )
)

head(data2)
meds
