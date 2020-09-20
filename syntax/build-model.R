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
library(broom)        # for tidying model output
library(stargazer)    # for formatting model table

load("./data/derived/data-for-analysis.RData")




# ========== 2. model - time only ===========

# ========== 2.1 fit model with general time ===========
model_time_general <- 
  glm(breakoff_status ~ factor(question_sequence) - 1,
      family = "binomial",
      data = data_for_model)


# ========== 2.2 fit model with polynomial time ===========
model_time_polynomial <-
  map(c(1, 2, 3, 4, 5),
      ~ glm(
        breakoff_status ~ poly(question_sequence, .x, raw = TRUE) - 1,
        family = "binomial",
        data = data_for_model)
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
data_for_model %>% distinct(case_id) %>% nrow()
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
data_for_model_1 <-   
  data_for_model %>% 
  select(case_id, all_of(vars_demographics)) %>% 
  na.omit()

# remove rows with missing data in the variables used in model 2
data_for_model_2 <-  
  data_for_model %>% 
  select(case_id, all_of(c(vars_demographics, vars_paradata))) %>% 
  na.omit()


# check the number of loss observations
# model with time only
data_for_model %>% nrow() # 229940
# model 1
data_for_model_1 %>% nrow() # 217077
# model 2
data_for_model_2 %>% nrow() # 217077

# number of loss observations (model with time only vs. model 1/2):
# 229940 - 217077 = 12863
# 12863 / 229940 * 100 = 5.59%


# check the number of loss respondents
# model with time only
data_for_model %>% distinct(case_id) %>% nrow() # 3128
# model 1
data_for_model_1 %>% distinct(case_id) %>% nrow() # 2732
# model 2
data_for_model_2 %>% distinct(case_id) %>% nrow() # 2732

# number of loss respondents (model with time only vs. model 1/2):
# 3128 - 2732 = 396
# 396 / 3128 * 100 = 12.66%


# conclusion:
# over 10% sample is loss when removing rows with missing data in the independent variables
# this loss of respondent size is not ignorable and needs to be noted


# ========== 3.3 create data for model 1 and 2 ===========
data_for_model_full <-
  data_for_model %>%
  select(case_id, breakoff_status, question_sequence,
         all_of(c(vars_demographics, vars_paradata))) %>%
  na.omit()


# ========== 3.4 fit model 1 (i.e. time + demographics) ===========
model_demographic_full <-
  list(
    as.formula(str_c("breakoff_status ~ question_sequence + I(question_sequence^2)",
                     " + ", str_c(vars_demographics, collapse = " + "))),
    as.formula(str_c("breakoff_status ~ question_sequence + I(question_sequence^2) + I(question_sequence^3)",
                     " + ", str_c(vars_demographics, collapse = " + ")))
  ) %>%
  map( ~ glm(.x, data = data_for_model_full,
             family = "binomial")) %>%
  set_names(c("time2", "time3"))


# ========== 3.5 fit model 2 (i.e. time + demographics + paradata) ===========
model_demographic_paradata_full <-
  model_demographic_full %>%
  map(~ update(.x, str_c(". ~ . + ", str_c(vars_paradata, collapse = "+"))))


# ========== 3.6 compare model by coefficient ===========
stargazer(model_demographic_full$time2,
          model_demographic_full$time3,
          model_demographic_paradata_full$time2,
          model_demographic_paradata_full$time3,
          type = "text",
          apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          intercept.bottom = FALSE)


# ========== 3.7 compare model by goodness-of-fit ===========
# compare square and cubic model

# model 1
anova(model_demographic_full$time2,
      model_demographic_full$time3)
# test statistic: 44.51

qchisq(0.95, df = 1) # 3.84

# conclusion:
# the cubic time model is preferred over the squared one 


# model 2
anova(model_demographic_paradata_full$time2,
      model_demographic_paradata_full$time3)
# test statistic: 8.79

qchisq(0.95, df = 1) # 3.84

# conclusion:
# the cubic time model is preferred over the squared one 


# compare model 1 and 2
anova(model_demographic_full$time3,
      model_demographic_paradata_full$time3)
# test statistic: 338.9

qchisq(0.95, df = 15) # 25.00

# check the change in aic
(AIC(model_demographic_full$time3) - AIC(model_demographic_paradata_full$time3)) / AIC(model_demographic_full$time3) * 100
# 7.87%


# conclusion:
# paradata helps improve the model fit and should be kept in the model
# in other words, model 2 is better than model 1


# report the number of respondents in model 1 and 2
data_for_model_full %>% distinct(case_id) %>% nrow() # 2732




# ========== 4. model 3 & 4 ===========

# ========== 4.1 create data for model 3 and 4 ===========
# create new variable of question topics 
data_for_model_restricted <-
  data_for_model %>%
  mutate(topic = fct_recode(topic,
                            NULL = "commitment page",
                            NULL = "demographics",
                            NULL = "housing",
                            NULL = "income"))

# check the result of re-coding         
data_for_model_restricted %>% pull(topic)

# remove rows/questions that are not in abc blocks
data_for_model_restricted <-
  data_for_model %>% 
  filter(str_detect(question_name, "^(q|d|e[0-9]|f[0-9])|ceintro", negate = TRUE))

# remove missing data in independent variables of model 3 and 4
data_for_model_restricted <-
  data_for_model_restricted %>% 
  select(case_id, breakoff_status, question_sequence,
         all_of(c(vars_demographics, vars_paradata))) %>% 
  na.omit()


# ========== 4.2 check breakoff rate in the randomised question blocks ===========
# calculate the breakoff rate in the randomised question blocks - all respondents
data_for_model_restricted %>% 
  group_by(case_id) %>% 
  filter(row_number() == n()) %>% 
  group_by(breakoff_status) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n) * 100)
# 168 / 2637 * 100 = 6.37%


# calculate the breakoff rate in the randomised question blocks - by filter question format
data_for_model_restricted %>% 
  group_by(case_id) %>% 
  filter(row_number() == n()) %>% 
  group_by(breakoff_status, grouped) %>% 
  summarise(n = n()) %>% 
  group_by(grouped) %>% 
  mutate(percent = n/sum(n) * 100)
# interleafed: 83 / 1353 * 100 =  6.13% 
# grouped: 85 / 1284 * 100 = 6.62%


# conclusion:
# still possible to analyse the data after restricting sample to only the three randomised question blocks


# ========== 4.3 fit model 3 (i.e. time + demographics + paradata) ===========
model_demographic_paradata_restricted <-
  list(
    as.formula(str_c("breakoff_status ~ question_sequence + I(question_sequence^2)",
                     " + ", str_c(vars_demographics, collapse = " + "),
                     " + ", str_c(vars_paradata[vars_paradata != "matrix"], collapse = " + "))),
    as.formula(str_c("breakoff_status ~ question_sequence + I(question_sequence^2) + I(question_sequence^3)",
                     " + ", str_c(vars_demographics, collapse = " + "),
                     " + ", str_c(vars_paradata[vars_paradata != "matrix"], collapse = " + ")))
  ) %>%
  map( ~ glm(.x, data = data_for_model_restricted,
             family = "binomial")) %>%
  set_names(c("time2", "time3"))


# ========== 4.4 fit model 4 (i.e. time + demographics + paradata + interaction) ===========
model_demographic_paradata_interaction_restricted <- 
  model_demographic_paradata_restricted %>%
  map2(
    .y = c(
      "question_sequence:grouped + I(question_sequence^2):grouped + question_sequence:topic + I(question_sequence^2):topic",
      "question_sequence:grouped + I(question_sequence^2):grouped + I(question_sequence^3):grouped + question_sequence:topic + I(question_sequence^2):topic + I(question_sequence^3):topic"
      ),
  ~ update(.x, str_c(". ~ . + ", .y)))


# ========== 4.5 compare model by coefficient ===========
stargazer(model_demographic_paradata_restricted$time2,
          model_demographic_paradata_restricted$time3,
          model_demographic_paradata_interaction_restricted$time2,
          model_demographic_paradata_interaction_restricted$time3,
          type = "text",
          apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          intercept.bottom = FALSE)


# ========== 4.6 compare models by goodness-of-fit ===========
# compare square and cubic time models

# model 3
anova(model_demographic_paradata_restricted$time2, 
      model_demographic_paradata_restricted$time3)
# test statistic: 3.22

qchisq(0.95, 1) # 3.84

# conclusion:
# the square time model is preferred over the cubic time model


# model 4
anova(model_demographic_paradata_interaction_restricted$time2,
      model_demographic_paradata_interaction_restricted$time3)
# test statistic: 9.73

qchisq(0.95, 4) # 9.49

# conclusion:
# the square time model may be preferred over the cubic time model


# compare model 3 and 4
anova(model_demographic_paradata_restricted$time2,
      model_demographic_paradata_interaction_restricted$time2)
# test statistic: 14.65

qchisq(0.95, 6) # 12.59

# conclusion:
# interaction helps improve the model fit and should be kept in the model
# in other words, model 4 is better than model 3


# report the number of respondents in model 3 and 4
data_for_model_restricted %>% distinct(case_id) %>% nrow() # 2637




# ========== 5. plot interaction effect between time and filter question format ===========
plot_interaction_effect <-   
  augment(model_demographic_paradata_interaction_restricted$time2) %>% 
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
       width = 18,
       height = 10,
       units = "cm")

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
  model_demographic_full,
  model_demographic_paradata_full,
  model_demographic_paradata_restricted,
  model_demographic_paradata_interaction_restricted,
  # path to save to
  file = "./data/derived/model.RData"
    )
    
    
    
