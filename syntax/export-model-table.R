# Table of Content
# 
# 1. prepare work space
#
# 2. report model 1 & 2
#   2.1 check names of the term/variable in model
#   2.2 create covariate label for model output table
#   2.3 get the number of respondents in model
#   2.4 create model output table
#
# 3. report model 3 & 4
#   2.1 check names of the term/variable in model
#   2.2 create covariate label for model output table
#   2.3 get the number of respondents in model
#   2.4 create model output table
#
# 4. report model - time only
#   4.1 check names of the term/variable in model
#   4.2 create covariate label for model output table
#   4.3 get the number of respondents in model
#   4.4 create model output table
#   4.5 create model goodness-of-fit comparison table
#
# 5. export model output table




# ========== 1. prepare work space ===========
library(tidyverse)    # for manipulating data
library(broom)        # for tidying model output
library(gt)           # for formatting table
library(modelsummary) # for formatting model table

load("./data/derived/model.RData")

# define style for model table output
apply_table_style <- 
  function (.gt_object, .column_start = 2, .column_end = 5) {
    
    .gt_object %>% 
      fmt_missing(columns = .column_start:.column_end, 
                  missing_text = "") %>% 
      text_transform(locations = cells_body(columns = .column_start:.column_end),
                     fn = function(.x) str_remove(.x, "\\(.+")) %>% 
      tab_style(locations = cells_column_labels(everything()),
                style = cell_text(weight = "bold")) %>% 
      tab_style(locations = cells_body(columns = everything(),
                                       rows = Variable == "Respondents"),
                style = cell_borders(color = "black",
                                     sides = "top")) %>% 
      tab_options(column_labels.border.top.color = "black",
                  column_labels.border.bottom.color = "black",
                  table_body.border.bottom.color = "black",
                  table_body.hlines.color = "white") %>% 
      tab_source_note("* p < 0.1, ** p < 0.05, *** p < 0.01") %>% 
      tab_options(table.border.bottom.color = "white") %>% 
      fmt(columns = .column_start:.column_end,
          rows = Variable %in% c("Observations",
                                 "AIC", "BIC", "Log Likelihood"),
          fn = function(.x) as.numeric(.x) %>% format(digits = 0, scientific = F, big.mark = ","))
  }
  

# ========== 2. report model 1 & 2 ===========

# ========== 2.1 check names of the term/variable in model ===========
model_demographic_full %>% 
  map( ~ tidy(.x)) %>% 
  walk( ~ print(.x, n = nrow(.x)))

model_demographic_paradata_full %>% 
  map( ~ tidy(.x)) %>% 
  walk( ~ print(.x, n = nrow(.x)))


# ========== 2.2 create covariate label for model output table ===========
covariate_label_full  <-
  c(
    "(Intercept)" = "Intercept",
    "question_sequence" = "Number of questions seen",
    "I(question_sequence^2)" = "Number of questions seen2",
    "I(question_sequence^3)" = "Number of questions seen3",
    "married_recodeYes" = "Married (ref: no)",
    "genderMale" = "Male (ref: female)",
    "age_recode" = "Age",
    "ethnicity_recodeNon-white" = "Non-white (ref: white)",
    "in_schoolYes" = "Current Student (ref: no)",
    "educ_recodeCollege" = "College",
    "educ_recodeBachelor and above" = "Bachelor or above",
    "household_incomeMiddle" = "Middle",
    "household_incomeHigh" = "High",
    "intro_pageYes" = "Introduction statement (ref: no)",
    "topicdemographics" = "Demographics",
    "topichousing" = "Housing",
    "topicclothing" = "Clothing",
    "topicutilities" = "Utilities",
    "topicinsurance" = "Insurance",
    "topicincome" = "Income",
    "matrixYes" = "Matrix question (ref: no)",
    "open_endedYes" = "Open-ended (ref: no)",
    "word_count" = "Question stem word count",
    "item_missing_percent" = "Item nonresponse rate",
    "groupedGrouped" = "Grouped (ref: Interleafed)",
    "deviceMobile device" = "Mobile device (ref: non-mobile)",
    "number_of_connectionMore than one session" = "Multiple sessions (ref: one session)",
    "duration_min" = "Survey duration (min)"
  )


# ========== 2.3 get the number of respondents in model ===========
n_respondent_model_full <-
  data_for_model_full %>%
  distinct(case_id) %>% 
  nrow() %>% 
  format(big.mark = ",")


# ========== 2.4 create model output table ===========
table_model_1_and_2  <- 
  modelsummary(list("Time2 + demographics" = model_demographic_full$time2,
                    "Time3 + demographics" = model_demographic_full$time3,
                    "Time2 + demographics + paradata" = model_demographic_paradata_full$time2,
                    "Time3 + demographics + paradata" = model_demographic_paradata_full$time3), 
               output = "gt",
               fmt = "%.2f",
               exponentiate = T, 
               stars = T,
               statistic_vertical = F,
               coef_map = covariate_label_full) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename("Variable" = " ") %>%
  add_row(Variable = "Education (ref: high school or below)",
          .before = str_which(.$Variable, "College")) %>% 
  add_row(Variable = "Household income (ref: low)",
          .before = str_which(.$Variable, "Middle")) %>% 
  add_row(Variable = "Topic (ref: commitment statement)",
          .before = str_which(.$Variable, "Demographics")) %>% 
  mutate(Variable = case_when(Variable == "Num.Obs." ~ "Observations",
                              Variable == "Log.Lik." ~ "Log Likelihood",
                              TRUE ~ Variable)) %>% 
  add_row(Variable = "Respondents",
          .before = str_which(.$Variable, "Observations")) %>% 
  print(n = nrow(.)) %>%
  gt() %>% 
  text_transform(locations = cells_body(columns = starts_with("Time"),
                                        rows = Variable == "Respondents"),
                 fn = function(.x) n_respondent_model_full) %>% 
  apply_table_style()


table_model_1_and_2




# ========== 3. report model 3 & 4 ===========

# ========== 3.1 check names of the term/variable in model ===========
model_demographic_paradata_restricted %>% 
  map( ~ tidy(.x)) %>% 
  walk( ~ print(.x, n = nrow(.x)))

model_demographic_paradata_interaction_restricted %>% 
  map( ~ tidy(.x) %>% 
         mutate(estimate = exp(estimate))) %>% 
  walk( ~ print(.x, n = nrow(.x)))


# ========== 3.2 create covariate label for model output table ===========
covariate_label_restricted  <-
  c(
    covariate_label_full,
    "question_sequence:groupedGrouped" = "Grouped x Questions seen",
    "I(question_sequence^2):groupedGrouped" = "Grouped x questions seen2",
    "I(question_sequence^3):groupedGrouped" = "Grouped x questions seen3",
    "question_sequence:topicutilities" = "Utilities x questions seen",
    "I(question_sequence^2):topicutilities" = "Utilities x questions seen2",
    "I(question_sequence^3):topicutilities" = "Utilities x questions seen3",
    "question_sequence:topicinsurance" = "Insurance x questions seen",
    "I(question_sequence^2):topicinsurance" = "Insurance x questions seen2",
    "I(question_sequence^3):topicinsurance " = "Insurance x questions seen3"
  )


# ========== 3.3 get the number of respondents in model ===========
n_respondent_model_restricted <- 
  data_for_model_restricted %>%
  distinct(case_id) %>% 
  nrow() %>% 
  format(big.mark = ",")


# ========== 3.4 create model output table ===========
table_model_3_and_4  <- 
  modelsummary(list("Time2 + demographics + paradata" = model_demographic_paradata_restricted$time2,
                    "Time3 + demographics + paradata" = model_demographic_paradata_restricted$time3,
                    "Time2 + demographics + paradata + interaction" = model_demographic_paradata_interaction_restricted$time2,
                    "Time3 + demographics + paradata + interaction" = model_demographic_paradata_interaction_restricted$time3), 
               output = "gt",
               fmt = "%.2f",
               exponentiate = T, 
               stars = T,
               statistic_vertical = F,
               coef_map = covariate_label_restricted) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename("Variable" = " ") %>%
  add_row(Variable = "Education (ref: high school or below)",
          .before = str_which(.$Variable, "College")) %>% 
  add_row(Variable = "Household income (ref: low)",
          .before = str_which(.$Variable, "Middle")) %>% 
  add_row(Variable = "Topic (ref: Demographics)",
          .before = str_which(.$Variable, "Utilities")) %>% 
  mutate(Variable = case_when(Variable == "Num.Obs." ~ "Observations",
                              Variable == "Log.Lik." ~ "Log Likelihood",
                              TRUE ~ Variable)) %>% 
  add_row(Variable = "Respondents",
          .before = str_which(.$Variable, "Observations")) %>% 
  print(n = nrow(.)) %>%
  gt() %>% 
  text_transform(locations = cells_body(columns = starts_with("Time"),
                                        rows = Variable == "Respondents"),
                 fn = function(.x) n_respondent_model_restricted) %>% 
  apply_table_style()


table_model_3_and_4




# ========== 4. report model - time only ===========

# ========== 4.1 check names of the term/variable in model ===========
model_time_polynomial %>% 
  map( ~ tidy(.x)) %>% 
  walk( ~ print(.x, n = nrow(.x)))


# ========== 4.2 create covariate label for model output table ===========
covariate_label_time_only  <-
  c(
    "poly(question_sequence, .x, raw = TRUE)" = "Number of questions seen",
    "poly(question_sequence, .x, raw = TRUE)1" = "Number of questions seen",
    "poly(question_sequence, .x, raw = TRUE)2" = "Number of questions seen2",
    "poly(question_sequence, .x, raw = TRUE)3" = "Number of questions seen3",
    "poly(question_sequence, .x, raw = TRUE)4" = "Number of questions seen4",
    "poly(question_sequence, .x, raw = TRUE)5" = "Number of questions seen5"
  )


# ========== 4.3 get the number of respondents in model ===========
n_respondent_model_time_only <- 
  data_for_model %>%
  distinct(case_id) %>% 
  nrow() %>% 
  format(big.mark = ",")


# ========== 4.4 create model output table ===========
table_model_time_polynomial <-   
  modelsummary(list("Linear time" = model_time_polynomial$time1,
                    "Squared time" = model_time_polynomial$time2,
                    "Cubic time" = model_time_polynomial$time3,
                    "4th" = model_time_polynomial$time4,
                    "5th" = model_time_polynomial$time5),
               output = "gt",
               fmt = "%.2f",
               exponentiate = T, 
               stars = T,
              statistic_vertical = F,
              coef_map = covariate_label_time_only) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename("Variable" = " ") %>%
  mutate(Variable = case_when(Variable == "Num.Obs." ~ "Observations",
                              Variable == "Log.Lik." ~ "Log Likelihood",
                              TRUE ~ Variable)) %>% 
  add_row(Variable = "Respondents",
          .before = str_which(.$Variable, "Observations")) %>% 
  print(n = nrow(.)) %>%
  gt() %>% 
  text_transform(locations = cells_body(columns = 2:6,
                                        rows = Variable == "Respondents"),
                 fn = function(.x) n_respondent_model_time_only) %>% 
  apply_table_style(.column_end = 6)


table_model_time_polynomial


# ========== 4.5 create model goodness-of-fit comparison table ===========
table_gof_comparison <- 
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
         bic_diff = lag(bic) - bic) %>% 
  mutate(model = c("Linear time", "Squared time", "Cubic time", "4th", "5th")) %>% 
  gt() %>% 
  cols_label(
    "model" = "Time specification",
    "deviance" = "Deviance",
    "aic" = "AIC",
    "bic" = "BIC",
    "deviance_diff" = "Deviance difference",
    "aic_diff" = "AIC difference",
    "bic_diff" = "BIC difference"
  ) %>% 
  fmt_missing(columns = everything(),
              missing_text = "-") %>% 
  fmt_number(columns = 2:7,
             decimals = 0) %>% 
  tab_style(locations = cells_column_labels(everything()),
            style = cell_text(weight = "bold")) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white")


table_gof_comparison





# ========== 5. export model output table ===========
walk2(
  .x = list(table_model_1_and_2,
            table_model_3_and_4,
            table_model_time_polynomial,
            table_gof_comparison),
  .y = list("table-model-1-and-2",
            "table-model-3-and-4",
            "table-model-time-polynomial",
            "table-gof-comparison"),
  ~ gtsave(.x,
           filename = str_c(.y, ".html"),
           path = str_c(rprojroot::find_rstudio_root_file(),"/doc"))
)

