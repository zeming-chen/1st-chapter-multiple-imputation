# Table of Content
#
# 1. prepare work space
#
# 2. recode breakoff status
#
# 3. recode number of times logging into the survey
#
# 4. recode ethnicity
#
# 5. recode household income
# 
# 6. recode education
#
# 7. recode student status
#
# 8. recode marital status
#
# 9. recode filter question format
#
# 10. recode responding device
#
# 11. recode age
#
# 12. recode gender
#
# 13. recode item nonresponse
#   13.1 recode item nonresponse in matrix question d25
#   13.2 recode item nonresponse in other questions
#   13.3 calculate the cumulative rate of item nonresponse
#
# 14. create data for descriptive analysis
#   14.1 create data for kaplan meier plot
#   14.2 create data for description
#
# 15. create data for modelling
#
# 16. export data




# ====== 1. prepare work space ======
library(tidyverse)

getwd()

load("./data/derived/data-dirty.RData")
load("./data/derived/data-meta.RData")

source("./syntax/customised function/replace-d25-answer.R")




# ====== 2. recode breakoff status =======
# examine the variable corresponding to the response status
data_interim %>% 
  count(resdisposition)
# find:
# there are 3 types of response status; they are:
# 1) "Completed"; 
# 2) "Dropped"; 
# 3) "Screened Out"
# 4) "Unused"
# two respondents had an "unused" response status
# these two cases will be removed from subsequent analysis

# identify the respondents with unknown response status
id_unknown_response_status <- 
  data_interim %>% 
  filter(resdisposition == "Unused") %>% 
  pull(nid)

# remove these two respondents from the survey data
data_recode <- 
  data_interim %>% 
  filter(!nid %in% id_unknown_response_status)

# remove these two respondents from long data as well
data_long <-
  data_long %>%
  group_by(case_id) %>%
  filter(!case_id %in% id_unknown_response_status) %>%
  ungroup()

# examine the response status "Screened Out"
data_recode %>% 
  filter(resdisposition == "Screened Out") %>% 
  count(resdisposition, lastquestionfilled)
# find:
# for all respondents with the "Screened Out" response status (68 in total), 
# their last question answered was Q1
# so all cases with "Screened Out" should be re-coded as "Breakoff"

# create a new column for breakoff status
data_recode <- 
  data_recode %>% 
  mutate(breakoff = if_else(resdisposition == "Completed", "No", "Yes"),
         breakoff = as_factor(breakoff))

# examine the column "breakoff"
data_recode %>% count(breakoff)
# find:
# there are 520 breakoffs and 2608 completing respondents
# the breakoff rate is 520/(520+2608) = 0.1662404 --> 17%




# ============ 3. recode number of times logging into the survey =============
table(data_recode$numberofconnections, useNA = "always")
# find:
# the majority of respondents finished/broke off the survey within one survey connection
# so the number of connection should be recoded as binary variable:
# value of 0 means connecting to survey just one time,
# while value of 1 means logging back into survey more than one time

data_recode <-
  data_recode %>%
  mutate(number_of_connection = case_when(numberofconnections == 1 ~ 0,
                                          TRUE ~ 1),
         number_of_connection = factor(number_of_connection, 
                                       levels = c(0, 1),
                                       labels = c("One session", "More than one session")))

data_recode %>% count(number_of_connection)




# ============ 4. recode ethnicity =============
# ethnicity is measured in the survey by multiple questions (i.e. matrix question)
# ethnicity is also recorded in the demographics data under the name "ethnicity"
# thus, need to check which ethnicity should be used
# one way is to check which source has the least missing data

# check number of missingness in the ethnicity-related variables in the survey
data_recode %>% 
  select(hispanic, white, matches("d25c[0-9]{1,2}$"), d25c9o) %>% 
  summarise(across(.cols = everything(), ~ is.na(.))) %>% 
  rowwise() %>% 
  transmute(number_of_na = sum(c_across(hispanic:d25c9o))) %>% 
  filter(number_of_na == 13)
# find:
# 200 missing

# check number of missingness in the ethnicity variable in the sampling frame
data_recode %>% 
  count(ethnicity)
# find:
# in total, 122 missing in the ethnicity variable in the sampling frame (details below)
# 49 have the value of NA
# there are 67 cases with level "0"
# 6 cases refuse to report their ethnicity

# conclusion: 
# because the ethnicity variable from sampling frame has fewer missing data
# it will be used in the subsequent analysis

data_recode <-
  data_recode %>%
  mutate(ethnicity_recode = case_when(ethnicity == "White" ~ "White",
                           ethnicity %in% c(NA, "0", "I prefer not to answer") ~ NA_character_,
                           TRUE ~ "Non-white"),
         ethnicity_recode = as_factor(ethnicity_recode))

data_recode %>% count(ethnicity_recode)




# ============ 5. recode household income =============
# household income is also measured by different variables
# and can come from different sources
# thus, need to check which household income variable should be used
# one way is to check which measure/source has the least missing data
data_recode %>% count(hhi) # 49
data_recode %>% count(hhinccat) # above 1359
data_recode %>% count(hhinccat_m) # 1469
data_recode %>% count(wageinc) # above 524
data_recode %>% count(wageinc_m) # 1587
# find:
# the above variables are all about incomes, 
# "hhi" is from the sampling frame
# the other four variables are from the survey data
# because only "hhi" has the fewest missing data (i.e. 49), 
# it will be used in the subsequent analysis

data_recode <- 
  data_recode %>% 
  mutate(household_income = as_factor(hhi),
         household_income = 
           fct_collapse(
             household_income, 
             "Low" = c("$0 - $14,999", "$15,000 - $24,999"),
             "Middle" = c("$25,000 - $49,999", "$50,000 - $79,999", "$80,000 - $99,999"),
             "High" = c("$100,000 - $149,999", "$150,000 - $199,999", "$200,000 or more")
             ),
         household_income = fct_relevel(household_income, "Low", "Middle", "High"))

data_recode %>% count(household_income)




# ============ 6. recode education =============
# education is also measured by different variables
# and can come from different sources
# thus, need to check which education variable should be used
# one way is to check which measure/source has the least missing data
data_recode %>% count(education) # 679
data_recode %>% count(educ) # 245
# find:
# there are two variables measuring education level, they are:
# "education" from demographic dataset and "educ" from the survey data
# "educ" has fewer missing data (i.e. 245)
# thus, the subsequent analysis will use "educ"
# in "educ", there are 7 cases coded as ".d" and 18 ".r" --> re-coded as NA later
# the newly education variable should have 3 ordered levels:
# (1) "High school or below"; (2) "College"; (3) "Bachelor and above"

data_recode <- 
  data_recode %>% 
  mutate(educ_recode = case_when(educ == ".d" | educ == ".r" ~ NA_character_,
                                  TRUE ~ as.character(educ)),
         educ_recode = as_factor(educ_recode),
         educ_recode = 
           fct_recode(
            educ_recode,
             "High school or below" = "High school (grades 9-12, no degree)" ,
             "High school or below" = "High school graduate - high school diploma or the equivalent (GED)",
             "College" = "Some college but no degree"   ,
             "College" = "Associate's degree in college"  ,
             "Bachelor and above" = "Bachelor's degree (BA, AB, BS, etc.)"  ,
             "Bachelor and above" = "Master's, professional, or doctorate degree (MA, MS, MBA, MD, JD, PhD, etc.)"),
         educ_recode = fct_rev(educ_recode))

data_recode %>% count(educ_recode)



# ============ 7. recode student status =============
data_recode %>% count(inschool)
# find:
# 255 missing

data_recode <- 
  data_recode %>% 
  mutate(in_school = factor(inschool,
                            levels = c("0", "1"),
                            labels = c("No", "Yes")))

data_recode %>% count(in_school)




# ============ 8. recode marital status =============
data_recode %>% count(married)
# find:
# 262 missing

data_recode <- 
  data_recode %>% 
  mutate(married_recode = factor(married,
                                 levels = c("0", "1"),
                                 labels = c("No", "Yes")))

data_recode %>% count(married_recode)




# ============ 9. recode filter question format =============
data_recode %>% count(grouped)
# find:
# 1544 grouped vs. 1584 interleafed

data_recode <-
  data_recode %>%
  mutate(grouped = as_factor(grouped))

data_recode %>% count(grouped)




# ============ 10. recode responding device =============
data_recode %>% count(device)
# find:
# 3 unknown devices --> re-cod as NA later
# 138 tablet users --> re-code as mobile device later

data_recode <-
  data_recode %>%
  mutate(device = case_when(device == "Unknown" ~ NA_character_,
                            device == "Desktop" ~ "Non-mobile device",
                            TRUE                ~ "Mobile device"),
         device = as_factor(device))

data_recode %>% count(device)




# ============ 11. recode age =============
# there are two age-related variables:
# "age.x" is from the survey data while "age.y" is from the sampling frame
# thus, need to check which age variable should be used
# one way is to check which measure/source has the least missing data
data_recode %>% 
  count(age.x) %>% 
  print(n = nrow(.))
# find:
# 253 missing

data_recode %>% 
  count(age.y) %>% 
  print(n = nrow(.))
# find:
# 49 missing

# conclusion:
# use "age.y" in the subsequent analysis because it has fewer missing cases

data_recode <- 
  data_recode %>% 
  mutate(age_recode = age.y)

data_recode %>% 
  select(age.y, age_recode) %>% 
  mutate(equal = age.y == age_recode) %>% 
  filter(equal != TRUE)




# ============ 12. recode gender =============
data_recode %>% count(gender)
# find:
# 49 missing

data_recode <- 
  data_recode %>% 
  mutate(gender = as_factor(gender))

data_recode %>% count(gender)




# ============ 13. recode item nonresponse ============

# ============ 13.1 recode item nonresponse in matrix question d25 =============
# in the long data,
# the original coding for the answer to matrix question d25 is wrong (proof below)
data_long %>% 
  filter(question_name == "d25") %>% 
  count(answer)
# find:
# there are too many missing data (i.e. 2887)
# this large number of missing data is the by-product when cleaning the long data
# thus, this mistake needs to be corrected

# get the correct number of item nonresponse in question d25
data_recode$d25_na <-
  data_recode %>%
  select(white:d25c11) %>% 
  map_df( ~ !is.na(.x)) %>%
  # count the number of answers given to the matrix question d25
  # value "0" means the respondent did not answer any question in the matrix d25
  transmute(number_of_answers_in_d25 = rowSums(.),
            number_of_answers_in_d25 = as.character(number_of_answers_in_d25)) %>% 
  pull()

# check the result of re-coding
data_recode %>% 
  count(d25_na)
# find:
# 205 respondents did not answer the matrix question d25 at all

# check why there is a large amount of item nonresponse in matrix d25
data_recode %>% 
  filter(d25_na == "0") %>% 
  count(last_question_filled)
# find: 
# respondents do not answer question d25 ENTIRELY because they broke off before d25

# replace the wrong answer to d25 in the long data with the correct one
data_long <- 
  data_long %>% 
  split(.$case_id) %>% 
  map_df( ~ replace_d25_answer(.))

# check the result of re-coding
data_long %>% 
  filter(question_name == "d25") %>% 
  count(answer)
# find:
# answers to d25 do not have missing data now


# ============ 13.2 recode item nonresponse in other questions =============
data_long <-
  data_long %>%
  mutate(answer = if_else(answer %in% c(".r", ".d", ".a"), NA_character_, answer),
         answer = case_when(question_name %in% c("a1", "b1", "c1") ~ "not missing",
                            TRUE ~ answer))


# ============ 13.3 calculate the cumulative rate of item nonresponse =============
data_long <-
  data_long %>% 
  mutate(item_missing = if_else(answer %in% NA, 1, 0)) %>% 
  group_by(case_id) %>% 
  mutate(item_missing_cumsum = cumsum(item_missing),
         item_missing_percent = item_missing_cumsum/question_sequence * 100) %>% 
  ungroup()




# ============ 14. create data for descriptive analysis =============

# ====== 14.1 create data for kaplan meier plot =======
data_for_km_plot <-
  data_long %>%
  group_by(case_id) %>%
  slice_tail() %>%
  ungroup() %>% 
  mutate(filter_format = factor(filter_format))
  
data_for_km_plot %>% glimpse()


# ====== 14.2 create data for description =======
data_for_descriptive_analysis <- 
  data_recode %>% 
  select(case_id = nid,
         breakoff,
         grouped,
         number_of_connection,
         device,
         duration_min,
         married_recode,
         gender,
         age_recode,
         ethnicity_recode,
         in_school,
         educ_recode,
         household_income) %>% 
  add_column(question_sequence = data_for_km_plot$question_sequence,
             .after = "breakoff")

data_for_descriptive_analysis %>% glimpse()




# ============ 15. create data for modelling =============
# put all variables into one dataset for modelling
data_for_model <-
  data_long %>%
  group_by(case_id) %>%
  # merge the data of question characteristics (time-varying) into the long data
  left_join(data_meta, by = "question_name") %>%
  # merge the data of demographics (time-constant) into the above data
  left_join(data_for_descriptive_analysis %>% select(-question_sequence), 
            by = "case_id") %>% 
  ungroup()




# ============ 16. export data =============
save(data_meta,
     data_long, 
     data_for_km_plot,
     data_for_descriptive_analysis,
     data_for_model,
     file = "./data/derived/data-for-analysis.RData")


