# Table of Content
#
# 1. prepare work space
#
# 2. impute missing demographics
#   2.1 transform data for multiple imputation
#   2.2 check the missing data pattern
#   2.3 perform multiple imputation
#   2.4 check imputation convergence
#   2.5 compare origin data and imputed data - numeric
#   2.6 compare origin data and imputed data - categorical
#
# 3. create data for modelling
#
# 4. export data




# ====== 1. prepare work space ======
library(tidyverse)
library(mice)

getwd()

load("./data/derived/data-for-imputation.RData")

source("./syntax/customised function/describe-data-numeric.R")

# save the data with NA for later comparison with the imputed data
data_for_descriptive_analysis_raw <- data_for_descriptive_analysis


# ============ 2. impute missing demographics =============

# ============ 2.1 transform data for multiple imputation =============
# select the time-varying variables 
data_to_transform <- 
  data_long %>%
  group_by(case_id) %>% 
  left_join(data_meta, by = "question_name") %>% 
  select(case_id, item_missing_percent:word_count)


# transform the time-varying variables to wide format one by one
# word count, item nonresponse rate (mean)
data_transformed <-
  data_to_transform %>% 
  summarise(across(c(word_count, item_missing_percent),
                   ~ mean(.x),
                   .names = "mean_{.col}"))

# open-ended questions, intro page, matrix (sum)
data_transformed <- 
  left_join(x = data_transformed,
            y = data_to_transform %>% 
              select(case_id, open_ended, intro_page, matrix) %>%
              mutate(across(c(open_ended, intro_page, matrix),
                            ~ as.numeric(.x) - 1,
                            .names = "sum_{.col}")) %>% 
              mutate(across(starts_with("sum"), ~ sum(.x))) %>% 
              slice_tail() %>% 
              select(case_id, starts_with("sum")) %>% 
              ungroup(),
            by = "case_id")

# topic (sum)
data_transformed <- 
  left_join(x = data_transformed,
            y = data_long %>%
              group_by(case_id) %>% 
              left_join(data_meta, by = "question_name") %>% 
              select(case_id, topic) %>% 
              group_by_all() %>% 
              count() %>% 
              ungroup() %>% 
              pivot_wider(id_cols = "case_id",
                          names_from = topic,
                          names_prefix = "sum_",
                          values_from = n,
                          values_fill = 0) %>% 
              rename(sum_commitment_page = `sum_commitment page`),
            by = "case_id")


data_transformed


# add transformed time-varying variables to the data to be imputed
data_with_na <-
  data_for_descriptive_analysis %>% 
  left_join(data_transformed, by = "case_id")


# add question block order to the data to be imputed
data_with_na <- 
  data_with_na %>% 
  left_join(data_interim %>% select(nid, block_order), 
            by = c("case_id" = "nid")) %>% 
  select(-case_id)

data_with_na <- 
  data_with_na %>% 
  mutate(block_order = factor(block_order))


# ============ 2.2 check the missing data pattern =============
data_with_na %>% md.pattern(plot = F)
# find:
# education, student status and marital status have the largest missing data
# to be exactly, around 250 for each of these three variables
# ethnicity has around 120 missing
# gender, age and household income each have around 50 missing


# ============ 2.3 perform multiple imputation =============
# impute the data
list_imputed <- 
  mice(data_with_na, 
       m = 10,
       maxit = 50,
       seed = 2020, 
       print = F)


# ============ 2.4 check imputation convergence =============
plot(list_imputed)
# find:
# mean of imputed variables varies quite large across 10 imputed datasets

# get the number of imputed dataset
n_imputed_data <- list_imputed$m

# get the imputed data
data_imputed <- 
  list_imputed %>% 
  complete("all") %>% 
  set_names(str_c("impute_", 1:n_imputed_data)) %>% 
  map( ~ .x %>% as_tibble())


# ============ 2.5 compare origin data and imputed data - numeric =============
# get the distribution of the numeric variable - origin data
data_with_na %>% 
  select_if(is.numeric) %>% 
  select_if( ~ any(is.na(.))) %>% 
  map_df( ~ describe_data_numeric(.x), .id = "variable")


# get the distribution of the numeric variable - imputed data
data_imputed %>% 
  map( ~ .x %>% pull(age_recode)) %>% 
  map_df( ~ .x %>% describe_data_numeric(), .id = "impute_id")


# conclusion:
# distribution of age is not different between origin and imputed data


# ============ 2.6 compare origin data and imputed data - categorical =============
# get the distribution of the categorical variable - origin data
table_distribution_with_na <-
  data_with_na %>% 
  select_if(is.factor) %>%
  select_if( ~ any(is.na(.))) %>% 
  map_df( ~ table(.x, useNA = "always") %>% as_tibble(),
          .id = "variable") %>% 
  rename(category = ".x") %>% 
  group_by(variable) %>% 
  mutate(prop = n / sum(n) * 100) %>% 
  ungroup()

# get the distribution of the categorical variable - imputed data
table_distribution_imputed <-
  data_imputed %>% 
  map( ~ .x %>% select_if(is.factor)) %>% 
  map( ~ .x %>% select(-c(breakoff, grouped, number_of_connection))) %>%   
  map( ~ .x %>% map_df( ~ table(., useNA = "always") %>% as_tibble(),
                        .id = "variable")) %>% 
  map( ~ .x %>% rename(category = ".")) %>% 
  map( ~ .x %>% group_by(variable)) %>% 
  map( ~ .x %>% mutate(prop = n / sum(n) * 100)) %>%   
  map( ~ .x %>% ungroup())

# calculate the change in the percentage of categories for each categorical variable
table_to_compare_origin_and_impute_data <- 
  map2(
    .x = list(table_distribution_with_na) %>% rep(n_imputed_data),
    .y = table_distribution_imputed,
    ~ left_join(.x, .y, by = c("variable", "category"))
  ) %>% 
  set_names(str_c("impute_", 1:n_imputed_data)) %>% 
  map( ~ .x %>% 
         filter(!is.na(category)) %>% 
         group_by(variable) %>% 
         mutate(diff_origin = prop.x - lead(prop.x),
                diff_impute = prop.y - lead(prop.y))) %>% 
  map_dfr(~ .x, .id = "impute_id") %>% 
  ungroup()

# check the change in the percentage of categories for each categorical variable
table_to_compare_origin_and_impute_data %>% distinct(variable)

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "device")
# find:
# there is no much difference between imputed data and origin data

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "married_recode")
# find:
# imputed data makes the difference bigger

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "gender")
# find:
# there is no much difference between imputed data and origin data

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "ethnicity_recode")
# find:
# there is no much difference between imputed data and origin data

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "in_school")
# find:
# imputed data makes the difference smaller

table_to_compare_origin_and_impute_data %>% 
  filter(variable == "educ_recode") %>% 
  print(n = 50)
# find:
# imputed data makes the difference smaller


table_to_compare_origin_and_impute_data %>% 
  filter(variable == "household_income") %>% 
  print(n = 50)
# find:
# there is no much difference between imputed data and origin data


# conclusion:
# the distribution of three demographic variables is affected noticeably:
# marital status, education, student status




# ============ 3. create data for modelling =============
# overwrite the imputed data into the data used for descriptive summary later
data_for_descriptive_analysis <-
  map2(
    .x = data_for_descriptive_analysis_raw %>% select(case_id:duration_min) %>% list() %>% rep(n_imputed_data),
    .y = data_imputed %>% map( ~ .x %>% select(married_recode:household_income)),
    ~ bind_cols(.x, .y)
  ) %>%
  set_names(str_c("impute_", 1:n_imputed_data))


# put all variables into one dataset for modelling
data_for_model <-
  # merge the data of question characteristics (time-varying) into the long data
  map2(
    .x = data_long %>% group_by(case_id) %>% list() %>% rep(n_imputed_data),
    .y = data_meta %>% list() %>% rep(n_imputed_data),
    ~ left_join(.x, .y, by = "question_name")
  ) %>% 
  # merge the data of demographics (time-constant) into the above data
  map2(
    .y = data_for_descriptive_analysis %>% map( ~ .x %>% select(-question_sequence)), 
    ~ left_join(.x, .y, by = "case_id") %>% 
      ungroup()
  )

data_for_model <- 
  data_for_model %>% set_names(str_c("imputed_", 1:n_imputed_data))


# ============ 4. export data =============
save(data_meta,
     data_long, 
     data_for_km_plot,
     data_for_descriptive_analysis_raw,
     data_for_descriptive_analysis,
     data_for_model,
     file = "./data/derived/data-for-analysis.RData")
