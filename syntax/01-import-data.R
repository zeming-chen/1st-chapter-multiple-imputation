#   Table of Content
#   
#   1. prepare the work space
#
#   2. import data
#     2.1 import survey dataset
#     2.2 import sampling frame dataset (i.e. demographics)
#     2.3 import questionnaire pdf
#     2.4 rename columns
#
#   3. merge data
#     3.1 check the uniqueness of primary key used in data merging
#     3.2 check the completeness of primary key used in data merging
#     3.3 check the sample size of data to be merged
#     3.4 merge data_demographics into data_raw
#     3.5 check the final merged data
#
#   4. export data




# ================== 1. prepare the work space ==================
getwd()

library(tidyverse)    # for manipulating data
library(readxl)       # for importing excel data
library(pdftools)     # for importing pdf data




# ================== 2. import data ==================

# ================== 2.1 import survey dataset ==================
data_raw <- read_csv("./data/raw/2020-03-10 - data - lightspeed panel.csv")


# ================== 2.2 import sampling frame dataset ==================
file_path <- "./data/raw/2020-01-26 - data - demographics of lightspeed panel.xlsx"

data_demographics <- 
  file_path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map_dfr( ~ read_excel(.x, path = file_path))


# ================== 2.3 import questionnaire pdf ==================
data_questionnaire <- 
  pdf_text("./data/raw/2019-11-24 - lightspeed panel questionnaire.pdf")


# ================== 2.4 rename columns ==================
data_raw <- 
  data_raw %>% 
  rename_all( ~ str_to_lower(.x))


data_demographics <- 
  data_demographics %>% 
  rename_all( ~ str_to_lower(.x)) %>% 
  rename(hh_size = "hh size")




# ================== 3. merge data ==================

# ================== 3.1 check the uniqueness of primary key used in data merging ==================
# check the primary key in data_demographics
data_demographics %>% 
  count(id) %>% 
  filter(n > 1)
# find: 
# column "id" can't uniquely identify a respondent
# however, because it is the only primary key in data_demographics
# we have to use it as the primary key in merging the data
# strcture of values in the column "id": a combination of letters and numbers (e.g., 81dba1ec)


# check the primary key in data_raw
data_raw %>% 
  select(contains("id"))
# find:
# there are three id-related columns in the survey dataset: "caseid", "id", "nid"

# check the difference among these three columns
data_raw %>% select(caseid)
data_raw %>% select(id) 
data_raw %>% select(nid)
# find:
# values in the column "id" consist of letters and numbers (e.g., 81dba1ec)
# both columns "caseid" and "nid" consist of only numeric values (e.g, 22)

# check which id-related column can uniquely identify a respondent in data_raw
data_raw %>% count(caseid) %>% arrange(desc(n))
data_raw %>% count(id) %>% arrange(desc(n))
data_raw %>% count(nid) %>% arrange(desc(n))
# find:
# only column "nid" can uniquely identify a respondent

# conclusion:
# ideally, the column "nid" should be used in data merging because it can uniquely identify a respondent
# however, I have to use the column "id" in data merging
# because only "id" in data_raw have values that are a combination of letters and numbers (e.g., 81dba1ec)
# which has the same structure of the column "id" in data_demographics
# so, I will use the column "id" as the primary key when merging data_demographics into data_raw

# however, using a primary key that is not unique can cause problems in data merging:
# when adding columns from data_demographics to data_raw, I have a many-to-many relationship between the datasets
# as a further result, the final merged data will have some duplicated rows
# the table below shows all four possible sources of duplication in the final merged data

# |----------------------------------|----------------------------------|--------------------------------------|
# |                                  | "id" in data_demographics unique | "id" in data_demographics NOT unique |
# |----------------------------------|----------------------------------|--------------------------------------|
# | "id" in data_raw unique          | Situation 1                      | Situation 3                          |
# | "id" in data_raw data NOT unique | Situation 2                      | Situation 4                          |
# |----------------------------------|----------------------------------|--------------------------------------|

# Situation 1
# both datasets have unique "id"
# no duplication will happen in this situation


# Situation 2
# because "id" in data_demographics is unique
# no row will be duplicated in the final merged data even though "id" in data_raw is not unique
# it is just that for respondents who have the same "id" in data_raw
# they will have the same demographic info in the final merged data
# this is the assumption I have to make


# Situation 3 
# for respondents with unique "id" in data_raw but duplicated "id" in data_demographics
# they will see duplication in the final merged data
# then, I have to think of a way to remove this duplication
# luckily, the column "nid" in data_raw can uniquely identify a respondent
# it means that I can write codes to select only the first row for each "nid" in the final merged data
# by doing so, the duplication resulted from Situation 3 will be removed in the final merged data
# however, doing this has one requirement:
# for respondents with unique "id" in data_raw but duplicated "id" in data_demographics,
# they must have identical demographic information in the data_demographics
# in other words, it requires that rows of each duplicated "id" in data_demographics should be exactly the same (i.e. have same values in each column of data_demographics)

# now, a question arises:
# do respondents with duplicated "id" in data_demographic have identical demographic info?
# the codes below check out the answer to this question:
# 1st step: identify those respondents with duplicated "id" in data_demographics
repeated_id_in_demographic_data <- 
  data_demographics %>%
  count(id) %>%
  filter(n > 1) %>%
  pull(id)
# 2nd step: check whether duplicated "id" in data_demographics have identical demographic info
data_demographics %>% 
  filter(id %in% repeated_id_in_demographic_data) %>% 
  split(.$id) %>% 
  map( ~ distinct(.x)) %>% 
  map_dbl( ~ nrow(.x)) %>% 
  keep(~ .x > 1)
# find:
# respondents with duplicated "id" in data_demographics have identical demographic info
# therefore, to remove duplications in the final merged data resulted from Situation 3
# I can write some codes, telling R to only select the first row for each "nid" in the final merged data


# Situation 4
# as being proved above, each duplicated "id" in data_demographics has the same demographic info
# it means that the duplicated rows in the final merged data will have the same demographic info as well
# therefore, we can repeat what we do for Situation 3, that is:
# write some codes, telling R to only select the first row for each "nid" in the final merged data


# ================== 3.2 check the completeness of primary key used in data merging ==================
data_demographics %>% 
  filter(is.na(id))
# find: 
# no "id" is missing in data_demographics

data_raw %>% 
  filter(is.na(id)) %>% 
  nrow()
# find:
# 3 respondents in data_raw do not have "id"
# these 3 respondents will have missing demographic info in the final merged data


# ================== 3.3 check the sample size of data to be merged ==================
# check the number of row
nrow(data_raw) # 3130
nrow(data_demographics) # 3094
nrow(data_raw) - nrow(data_demographics) # 36
# find:
# the survey dataset (i.e. data_raw) has 36 more respondents than the sampling frame dataset (i.e. data_demographics)
# it means some respondents of the survey are not the original panel members
# I can only explain this by supposing 2 scenarios:
# scenario 1: in some households that registered with the panel, more than one household members answered the survey at the end
# scenario 2: the sampling frame is outdated (i.e. people who newly join the panel are not updated in the sampling frame)

# check whether every respondent in data_raw is also in data_demographics
anti_join(data_raw, data_demographics, by = "id") %>% 
  pull(id) %>%
  length()
# find:
# 45 respondents in the data_raw don't exist in the data_demographics
# it means these 45 cases will have missing demographic info in the final merged data
# the outdated sampling frame, again, may be the reason for these 45 cases

# check the number of column
ncol(data_raw) # 326
ncol(data_demographics) # 7


# conclusion:
# I expect the final merged dataset to have 3130 rows, 332 columns
# and at least 45 respondents will have missing demographic info


# ================== 3.4 merge data_demographics into data_raw ==================
data_interim <- 
  left_join(data_raw, data_demographics, by = "id")

# select only the first row for every unique case in the final merged dataset to remove duplication
data_interim <- 
  data_interim %>% 
  group_by(nid) %>% 
  slice(1) %>%
  ungroup()


# ================== 3.5 check the final merged data ==================
data_interim %>% glimpse()
# find:
# there are 3130 respondents and 332 variables in the final merged data
# which conforms to my previous expectation

# check the number of respondents with missing data in variables from data_demographics (i.e. sampling frame data)
data_interim %>% 
  select(gender:hhi) %>% 
  mutate(across(.cols = everything(), ~ is.na(.x), .names = "{.col}_na")) %>% 
  rowwise() %>% 
  mutate(n_na = sum(c_across(gender_na:hhi_na))) %>% 
  count(n_na)
# 49 respondents in the final merged data do not have any data in the demographic variables from the sampling frame
# my previous expectation is 45
# the difference of 4 is purely because 4 respondents in the sampling frame already have missing data (proof is in the code below)
data_demographics %>% 
  mutate(across(.cols = everything(), ~ is.na(.x), .names = "{.col}_na")) %>% 
  rowwise() %>% 
  mutate(n_na = sum(c_across(gender_na:hhi_na))) %>% 
  count(n_na)


# conclusion:
# every aspect of the final merged data confirms to my previous expectation
# thus, this data is ready to be exported for future use




# ================== 4. export data ==================
save(data_interim, data_questionnaire, file = "./data/derived/data-question-sequence-out-of-order.RData")

