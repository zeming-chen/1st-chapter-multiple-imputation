# Table of Content
# 
# 1. prepare work space
#
# 2. model - time only
#   2.1 fit model with general time
#   2.2 fit model with polynomial time
#   2.3 compare model by coefficients
#   2.4 compare model by goodness-of-fit
#
# 3. model 1 & 2
#   3.1 select independent variables
#   3.2 remove missing data in independent variables of model 1 and 2
#   3.3 create data for model 1 and 2
#   3.4 fit model 1 (i.e. time + demographics)
#   3.5 fit model 2 (i.e. time + demographics + paradata)
#   3.6 compare model by coefficient
#   3.7 compare model by goodness-of-fit
#
# 4. model 3 & 4
#   4.1 create data for model 3 and 4
#   4.2 check breakoff rate in the randomised question blocks
#   4.3 fit model 3 (i.e. time + demographics + paradata)
#   4.4 fit model 4 (i.e. time + demographics + paradata + interaction)
#   4.5 compare model by coefficient
#   4.6 compare model by goodness-of-fit
#
# 5. plot interaction effect between time and filter question format
#
# 6. export model and plot




# ========== 1. prepare work space ===========
getwd()

library(tidyverse)    # for manipulating data
library(mice)         # for manipulating multiple imputed data
library(broom)        # for tidying model output
library(gt)           # for formatting model table

load("./data/derived/data-for-analysis.RData")

source("./syntax/customised function/pool-mice-model-result.R")


# ========== 2. model - time only ===========

# ========== 2.1 fit model with general time ===========
model_time_general <- 
  glm(breakoff_status ~ factor(question_sequence) - 1,
      family = "binomial",
      data = data_for_model$imputed_1)


# ========== 2.2 fit model with polynomial time ===========
model_time_polynomial <-
  map(c(1, 2, 3, 4, 5),
      ~ glm(
        breakoff_status ~ poly(question_sequence, .x, raw = TRUE) - 1,
        family = "binomial",
        data = data_for_model$imputed_1)
  ) %>% 
  # give names for the list elements for readability and later reference
  set_names(str_c("time", 1:5))


# ========== 2.3 compare model by coefficients ===========
map_df(model_time_polynomial, 
       ~ tidy(.x), 
       .id = "model") %>% 
  select(-statistic) %>% 
  mutate_at(vars(estimate, std.error), ~ round(., 2)) %>% 
  mutate_at(vars(p.value), ~ round(., 4))
# find:
# coefficients of time^3 and higher are always significant


# ========== 2.4 compare model by goodness-of-fit ===========
map_df(model_time_polynomial, 
       ~ glance(.x) %>% 
         select(null.deviance, deviance, aic= AIC, bic = BIC) %>% 
         mutate(n = nobs(.x)), 
       .id = "model") %>% 
  # columns "null.deviance" and "n" are the same for all models --> remove these two columns
  select(-null.deviance, -n) %>%
  # calculate the change in the model fit
  mutate(deviance_diff = lag(deviance) - deviance,
         aic_diff = lag(aic) - aic,
         bic_diff = lag(bic) - bic)

# report the number of respondents in the time-only models
data_for_model$imputed_1 %>% distinct(case_id) %>% nrow()
# find: 3128




# ========== 3. model 1 & 2 ===========

# ========== 3.1 select independent variables ===========
vars_demographics <- 
  c("married_recode", 
    "gender", 
    "age_recode", 
    "ethnicity_recode", 
    "in_school", 
    "educ_recode", 
    "household_income")

vars_paradata <- 
  c("intro_page", 
    "topic", 
    "matrix", 
    "open_ended", 
    "word_count", 
    "item_missing_percent", 
    "grouped", 
    "device", 
    "number_of_connection", 
    "duration_min")


# ========== 3.2 remove missing data in independent variables of model 1 and 2 ===========
# remove rows with missing data in the variables used in model 1

# because model 1 is purely built upon demographic variables
# which do not have any missing data after imputation
# thus, using na.omit() does not drop any observations (proof below)

# number of observations - before using na.omit() for demographic variables
data_for_model %>% 
  map( ~ .x %>% select(case_id, all_of(vars_demographics))) %>% 
  map_dbl( ~ nrow(.)) %>% 
  unique()
# find: 229940

# number of observations - after using na.omit() for demographic variables
data_for_model %>% 
  map( ~ .x %>% select(case_id, all_of(vars_demographics))) %>% 
  map( ~ .x %>% na.omit()) %>% 
  map_dbl( ~ nrow(.)) %>% 
  unique()
# find: 229940


# remove rows with missing data in the variables used in model 2
data_for_model_2 <-  
  data_for_model %>% 
  map( ~ .x %>% select(case_id, all_of(c(vars_demographics, vars_paradata)))) %>% 
  map( ~ .x %>% na.omit()) %>% 
  na.omit()


# check the number of loss observations
# model with time only
data_for_model %>% map_dbl( ~ .x %>% nrow()) %>% unique() # 229940
# model 2
data_for_model_2 %>% map_dbl( ~ .x %>% nrow()) %>% unique() # 229863

# number of loss observations (model with time only vs. model 2):
# 229940 - 229863 = 77
# 77 / 229940 * 100 = 0.03%


# check the number of loss respondents
# model with time only
data_for_model %>% 
  map( ~ .x %>% distinct(case_id)) %>% 
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()
# 3128

# model 2
data_for_model_2 %>% 
  map( ~ .x %>% distinct(case_id)) %>% 
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()
# 3125

# number of loss respondents (model with time only vs. model 2):
# 3128 - 3125 = 3
# 3 / 3128 * 100 = 0.10%


# conclusion:
# an extremely tiny proportion observations/respondents is lost when removing rows with missing data in the independent variables
# this loss size is ignorable


# ========== 3.3 create data for model 1 and 2 ===========
data_for_model_full <-
  data_for_model %>%
  map( ~ .x %>% select(case_id, breakoff_status, question_sequence,
                       all_of(c(vars_demographics, vars_paradata)))) %>% 
  map( ~ .x %>% na.omit())
  

# ========== 3.4 fit model 1 (i.e. time + demographics) ===========
# get the number of imputed dataset
n_imputed_data <- length(data_for_model_full)

# squared time model
model_time2_demographic_full <- 
  map2(
    .x = str_c("breakoff_status ~ question_sequence + I(question_sequence^2)",
               " + ", str_c(vars_demographics, collapse = " + ")) %>% 
         rep(n_imputed_data) %>% 
         map( ~ as.formula(.x)),
    
    .y = data_for_model_full,
     
    ~ glm(.x, data = .y, family = "binomial"))


model_time2_demographic_full <- 
  model_time2_demographic_full %>% 
  set_names(str_c("time2_", "impute_", 1:n_imputed_data))


# cubic time model
model_time3_demographic_full <- 
  map2(
    .x = str_c("breakoff_status ~ question_sequence + I(question_sequence^2) + I(question_sequence^3)",
               " + ", str_c(vars_demographics, collapse = " + ")) %>% 
      rep(n_imputed_data) %>% 
      map( ~ as.formula(.x)),
    
    .y = data_for_model_full,
    
    ~ glm(.x, data = .y, family = "binomial"))


model_time3_demographic_full <- 
  model_time3_demographic_full %>% 
  set_names(str_c("time3_", "impute_", 1:n_imputed_data))


# ========== 3.5 fit model 2 (i.e. time + demographics + paradata) ===========
model_time2_demographic_paradata_full <- 
  map2(.x = model_time2_demographic_full,
       .y = str_c(". ~ . + ", str_c(vars_paradata, collapse = "+")),
       ~ update.formula(.x, .y)) %>% 
  map2(.y = data_for_model_full,
       ~ glm(.x, data = .y, family = "binomial")) %>%
  set_names(str_c("time2_", "impute_", 1:n_imputed_data))


model_time3_demographic_paradata_full <- 
  map2(.x = model_time3_demographic_full,
       .y = str_c(". ~ . + ", str_c(vars_paradata, collapse = "+")),
       ~ update.formula(.x, .y)) %>% 
  map2(.y = data_for_model_full,
       ~ glm(.x, data = .y, family = "binomial")) %>%
  set_names(str_c("time3_", "impute_", 1:n_imputed_data))


# ========== 3.6 compare model by coefficient ===========
left_join(x = model_time3_demographic_paradata_full %>% get_pooled_estiamte(),
          y = model_time2_demographic_paradata_full %>% get_pooled_estiamte(),
          by = "term") %>% 
  left_join(
    left_join(x = model_time3_demographic_full %>% get_pooled_estiamte(),
              y = model_time2_demographic_full %>% get_pooled_estiamte(),
              by = "term"),
    by = "term"
  ) %>% 
  rename_at(vars(ends_with(".x.x")), 
            ~ str_replace(., ".x.x", "_m2_time3")) %>% 
  rename_at(vars(ends_with(".y.x")), 
            ~ str_replace(., ".y.x", "_m2_time2")) %>% 
  rename_at(vars(ends_with(".x.y")), 
            ~ str_replace(., ".x.y", "_m1_time3")) %>% 
  rename_at(vars(ends_with(".y.y")), 
            ~ str_replace(., ".y.y", "_m1_time2")) %>% 
  select(term, 
         ends_with("_m1_time2"), ends_with("_m1_time3"),
         ends_with("_m2_time2"), ends_with("_m2_time3"),
         -starts_with("p.value")) %>% 
  gt() %>% 
  fmt_missing(columns = everything(),
              missing_text = "") %>% 
  tab_source_note("* p < 0.1, ** p < 0.05, *** p < 0.01")
  


# ========== 3.7 compare model by goodness-of-fit ===========
# compare square and cubic model

# model 1
D1(model_time3_demographic_full,
   model_time2_demographic_full)
# p value: 0.045

# conclusion:
# the cubic time model is preferred over the squared one 


# model 2
D1(model_time3_demographic_paradata_full,
   model_time2_demographic_paradata_full)
# p value: 0.18

# conclusion:
# the square time model is preferred over the cubic one 


# compare model 1 and 2
D1(model_time2_demographic_paradata_full,
   model_time2_demographic_full)
# p value < 0


# check the change in aic
map2(.x = model_time2_demographic_full %>% map( ~ AIC(.x)),
     .y = model_time2_demographic_paradata_full %>% map( ~ AIC(.x)),
      ~ .x - .y) %>% 
  map2_dbl(.y = model_time2_demographic_full %>% map( ~ AIC(.x)),
           ~ .x / .y * 100) %>% 
  mean()
# 21.34%


# conclusion:
# paradata helps improve the model fit and should be kept in the model
# in other words, model 2 is better than model 1


# report the number of respondents in model 1 and 2
data_for_model_full %>% map( ~ .x %>% distinct(case_id)) %>% map( ~ .x %>% nrow()) # 3125




# ========== 4. model 3 & 4 ===========

# ========== 4.1 create data for model 3 and 4 ===========
# create new variable of question topics 
data_for_model_restricted <-
  data_for_model %>%
  map( ~ .x %>% mutate(topic = fct_recode(topic,
                                          NULL = "commitment page",
                                          NULL = "demographics",
                                          NULL = "housing",
                                          NULL = "income")))

# check the result of re-coding         
data_for_model_restricted %>% map( ~ .x %>% pull(topic) %>% levels())

# remove rows/questions that are not in abc blocks
data_for_model_restricted <-
  data_for_model %>% 
  map( ~ .x %>% filter(str_detect(question_name, "^(q|d|e[0-9]|f[0-9])|ceintro", negate = TRUE)))

# check number of observations - before removing missing data using na.omit()
data_for_model_restricted %>% 
  map( ~ .x %>% distinct(case_id)) %>% 
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()
# 2798

# check number of respondents - before removing missing data using na.omit()
data_for_model_restricted %>% map_dbl( ~ .x %>% nrow()) %>% unique()
# 149248


# remove missing data in independent variables of model 3 and 4
data_for_model_restricted <-
  data_for_model_restricted %>% 
  map( ~ .x %>% 
         select(case_id, breakoff_status, question_sequence,
                all_of(c(vars_demographics, vars_paradata)))) %>% 
  map( ~ .x %>% na.omit())


# check number of observations - after removing missing data using na.omit()
data_for_model_restricted %>% map_dbl( ~ .x %>% nrow()) %>% unique()
# 149201

# number of loss observations
# 149248 - 149201 = 47
# 47 / 149248 * 100 = 0.03%


# check number of observations - after removing missing data using na.omit()
data_for_model_restricted %>% 
  map( ~ .x %>% distinct(case_id)) %>% 
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()
# 2797

# number of loss respondents
# 2798 - 2797 = 1


# ========== 4.2 check breakoff rate in the randomised question blocks ===========
# calculate the breakoff rate in the randomised question blocks - all respondents
data_for_model_restricted %>% 
  map_df( ~ .x %>% 
            group_by(case_id) %>% 
            filter(row_number() == n()) %>% 
            group_by(breakoff_status) %>% 
            summarise(n = n()) %>% 
            mutate(percent = n/sum(n) * 100),
          .id = "impute_id") %>% 
  print(n = nrow(.))
# 188 / 2797 * 100 = 6.72%


# calculate the breakoff rate in the randomised question blocks - by filter question format
data_for_model_restricted %>% 
  map_df( ~ .x %>% 
            group_by(case_id) %>% 
            filter(row_number() == n()) %>% 
            group_by(breakoff_status, grouped) %>% 
            summarise(n = n()) %>% 
            group_by(grouped) %>% 
            mutate(percent = n/sum(n) * 100),
          .id = "impute_id") %>% 
  print(n = nrow(.))
# interleafed: 89 / 1411 * 100 =  6.31% 
# grouped: 99 / 1386 * 100 = 7.14%


# conclusion:
# still possible to analyse the data after restricting sample to only the three randomised question blocks


# ========== 4.3 fit model 3 (i.e. time + demographics + paradata) ===========
# squared time model
model_time2_demographic_paradata_restricted <-
  map2(
    .x = str_c("breakoff_status ~ question_sequence + I(question_sequence^2)",
               " + ", str_c(vars_demographics, collapse = " + "),
               " + ", str_c(vars_paradata[vars_paradata != "matrix"], collapse = " + ")) %>% 
      rep(n_imputed_data) %>% 
      map( ~ as.formula(.x)),
    
    .y = data_for_model_restricted,
    
    ~ glm(.x, data = .y, family = "binomial"))


model_time2_demographic_paradata_restricted <- 
  model_time2_demographic_paradata_restricted %>% 
  set_names(str_c("time2_", "impute_", 1:n_imputed_data))


# cubic time model
model_time3_demographic_paradata_restricted <-
  map2(
    .x = str_c("breakoff_status ~ question_sequence + I(question_sequence^2) + I(question_sequence^3)",
               " + ", str_c(vars_demographics, collapse = " + "),
               " + ", str_c(vars_paradata[vars_paradata != "matrix"], collapse = " + ")) %>% 
      rep(n_imputed_data) %>% 
      map( ~ as.formula(.x)),
    
    .y = data_for_model_restricted,
    
    ~ glm(.x, data = .y, family = "binomial"))


model_time3_demographic_paradata_restricted <- 
  model_time3_demographic_paradata_restricted %>% 
  set_names(str_c("time3_", "impute_", 1:n_imputed_data))


# ========== 4.4 fit model 4 (i.e. time + demographics + paradata + interaction) ===========
# square time model
model_time2_demographic_paradata_interaction_restricted <-
  map2(
    .x = model_time2_demographic_paradata_restricted,
    .y = ". ~ . + question_sequence:grouped + I(question_sequence^2):grouped + question_sequence:topic + I(question_sequence^2):topic",
    ~ update.formula(.x, .y)
  ) %>% 
  map2(.y = data_for_model_restricted,
       ~ glm(.x, data = .y, family = "binomial")) %>%
  set_names(str_c("time2_", "impute_", 1:n_imputed_data))


# cubic time model
model_time3_demographic_paradata_interaction_restricted <-
  map2(
    .x = model_time3_demographic_paradata_restricted,
    .y = ". ~ . + question_sequence:grouped + I(question_sequence^2):grouped + I(question_sequence^3):grouped + question_sequence:topic + I(question_sequence^2):topic + I(question_sequence^3):topic",
    ~ update.formula(.x, .y)
  ) %>% 
  map2(.y = data_for_model_restricted,
       ~ glm(.x, data = .y, family = "binomial")) %>%
  set_names(str_c("time3_", "impute_", 1:n_imputed_data))


# ========== 4.5 compare model by coefficient ===========
left_join(x = model_time3_demographic_paradata_interaction_restricted %>% get_pooled_estiamte(),
          y = model_time2_demographic_paradata_interaction_restricted %>% get_pooled_estiamte(),
          by = "term") %>% 
  left_join(
    left_join(x = model_time3_demographic_paradata_restricted %>% get_pooled_estiamte(),
              y = model_time2_demographic_paradata_restricted %>% get_pooled_estiamte(),
              by = "term"),
    by = "term"
  ) %>% 
  rename_at(vars(ends_with(".x.x")), 
            ~ str_replace(., ".x.x", "_m4_time3")) %>% 
  rename_at(vars(ends_with(".y.x")), 
            ~ str_replace(., ".y.x", "_m4_time2")) %>% 
  rename_at(vars(ends_with(".x.y")), 
            ~ str_replace(., ".x.y", "_m3_time3")) %>% 
  rename_at(vars(ends_with(".y.y")), 
            ~ str_replace(., ".y.y", "_m3_time2")) %>% 
  select(term, 
         ends_with("_m3_time2"), ends_with("_m3_time3"),
         ends_with("_m4_time2"), ends_with("_m4_time3"),
         -starts_with("p.value")) %>% 
  gt() %>% 
  fmt_missing(columns = everything(),
              missing_text = "") %>% 
  tab_source_note("* p < 0.1, ** p < 0.05, *** p < 0.01")


# ========== 4.6 compare models by goodness-of-fit ===========
# compare square and cubic time models

# model 3
D1(model_time3_demographic_paradata_restricted,
   model_time2_demographic_paradata_restricted)
# p value: 0.083

# conclusion:
# the square time model is preferred over the cubic time model


# model 4
D1(model_time3_demographic_paradata_interaction_restricted,
   model_time2_demographic_paradata_interaction_restricted)
# p value: 0.28

# conclusion:
# the square time model is preferred over the cubic time model


# compare model 3 and 4
D1(model_time2_demographic_paradata_interaction_restricted,
   model_time2_demographic_paradata_restricted)
# p value: 0.032

# conclusion:
# interaction helps improve the model fit and should be kept in the model
# in other words, model 4 is better than model 3


# report the number of respondents in model 3 and 4
data_for_model_restricted %>% map( ~ .x %>% distinct(case_id)) %>% map( ~ .x %>% nrow()) # 2797




# ========== 5. plot interaction effect between time and filter question format ===========
# check the plot of interaction effect for all models
model_time2_demographic_paradata_interaction_restricted %>% 
  map( ~ .x %>% augment()) %>% 
  map( ~ .x %>% mutate(fitted_prob = 1/(1 + exp(-.fitted)))) %>% 
  map( ~ .x %>% group_by(question_sequence, grouped)) %>% 
  map_df( ~ .x %>% summarise(mean_fitted_prob = mean(fitted_prob)),
          .id = "impute_id") %>% 
  ggplot(aes(x = question_sequence, y = mean_fitted_prob, linetype = grouped)) +
  geom_line() +
  facet_wrap( ~ impute_id, scales = "free_x") + 
  scale_x_continuous(breaks = seq(15, 195, 10), 
                     expand = c(0, 3)) + 
  labs(x = "Number of questions seen", y = "Fitted hazard of breakoff") + 
  scale_linetype_discrete(name = "Filter question format") + 
  theme_classic()
# find:
# all models show the same interaction pattern
# thus, only need to export one plot as an example


plot_interaction_effect <-   
  model_time2_demographic_paradata_interaction_restricted[[1]] %>% 
  augment() %>% 
  mutate(fitted_prob = 1/(1 + exp(-.fitted))) %>% 
  group_by(question_sequence, grouped) %>% 
  summarise(mean_fitted_prob = mean(fitted_prob)) %>% 
  ggplot(aes(x = question_sequence, y = mean_fitted_prob, linetype = grouped)) +
  geom_line() +
  scale_x_continuous(breaks = seq(15, 195, 10), 
                     expand = c(0, 3)) + 
  labs(x = "Number of questions seen", y = "Fitted hazard of breakoff") + 
  scale_linetype_discrete(name = "Filter question format") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = c(0.5, 0.85))


plot_interaction_effect




# ========== 6. export model and plot ===========
ggsave(plot_interaction_effect,
       filename = "plot-interaction-effect.png",
       path = "./plot",
       dpi = 300)
       # width = 18,
       # height = 10,
       # units = "cm")

save(
  # variable
  vars_demographics,
  vars_paradata,
  # data
  data_for_model,
  data_for_model_full,
  data_for_model_restricted,
  # model
  model_time_polynomial,
  model_time2_demographic_full,
  model_time3_demographic_full,
  model_time2_demographic_paradata_full,
  model_time3_demographic_paradata_full,
  model_time2_demographic_paradata_restricted, 
  model_time3_demographic_paradata_restricted,
  model_time2_demographic_paradata_interaction_restricted,
  model_time3_demographic_paradata_interaction_restricted,
  # path to save to
  file = "./data/derived/model.RData"
    )
    
