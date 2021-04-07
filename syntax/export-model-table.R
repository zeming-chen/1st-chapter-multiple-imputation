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
library(modelsummary) # for formatting model output table
library(knitr)
library(kableExtra)
library(gt)           # for formatting model output table
library(mice)         # for manipulating multiple imputed data

load("./data/derived/model.RData")

source("./syntax/customised function/pool-mice-model-result.R")

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

# get model's goodness of fit
get_model_gof <- function(.list_of_model, .n_respondent) {
  .list_of_model %>% 
    map_df( ~ .x %>% glance()) %>% 
    select(AIC, logLik, nobs) %>% 
    add_column(n_respondent = .n_respondent) %>% 
    summarise_all( ~ mean(.)) %>% 
    select(n_respondent, nobs, logLik, AIC) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate_all( ~ round(., 2)) %>% 
    mutate_all( ~ as.character(.x)) %>% 
    add_column(Variable = c("N of Respondents", "N of Observations", "Log Likelihood", "AIC"),
               .before = 1)
}



# ========== 2. report model 1 & 2 ===========

# ========== 2.1 check names of the term/variable in model ===========
model_time2_demographic_paradata_full$time2_impute_1 %>% 
  tidy() %>% 
  print(n = nrow(.)) %>% 
  pull(term)


# ========== 2.2 create covariate label for model output table ===========
covariate_label_full  <-
  c(
    "Intercept",
    "Number of questions seen",
    "Number of questions seen2",
    # "Number of questions seen3",
    "Married (ref: no)",
    "Male (ref: female)",
    "Age",
    "Non-white (ref: white)",
    "Current Student (ref: no)",
    "College",
    "Bachelor or above",
    "Middle",
    "High",
    "Introduction statement (ref: no)",
    "Demographics",
    "Housing",
    "Clothing",
    "Utilities",
    "Insurance",
    "Income",
    "Matrix question (ref: no)",
    "Open-ended (ref: no)",
    "Question stem word count",
    "Item nonresponse rate",
    "Grouped (ref: Interleafed)",
    "Mobile device (ref: non-mobile)",
    "Multiple sessions (ref: one session)",
    "Survey duration (min)"
  )


# ========== 2.3 get the number of respondents in model ===========
n_respondent_model_full <-
  data_for_model_full %>%
  map( ~ .x %>% distinct(case_id)) %>%
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()


# ========== 2.4 get the goodness of fit of model ===========
table_gof_model_1_and_2 <-
  map(list(model_time2_demographic_full,
           # model_time3_demographic_full,
           model_time2_demographic_paradata_full),
           # model_time3_demographic_paradata_full),
      ~ get_model_gof(.list_of_model = ., .n_respondent = n_respondent_model_full)) %>% 
  reduce(left_join, by = "Variable") %>% 
  rename_all(~ c("term", "estimate_m1_time2", "estimate_m2_time2"))


# ========== 2.5 get model output table ==========
table_model_1_and_2 <-
  left_join(x = model_time2_demographic_paradata_full %>% get_pooled_estiamte(),
            y = model_time2_demographic_full %>% get_pooled_estiamte(),
            by = "term") %>% 
  rename_at(vars(ends_with(".x")), 
            ~ str_replace(., ".x", "_m2_time2")) %>% 
  rename_at(vars(ends_with(".y")), 
            ~ str_replace(., ".y", "_m1_time2")) %>% 
  select(term, 
         ends_with("_m1_time2"), ends_with("_m2_time2"),
         -starts_with("p.value")) %>% 
  mutate(term = covariate_label_full) 
  
  # left_join(x = model_time3_demographic_paradata_full %>% get_pooled_estiamte(),
  #           y = model_time2_demographic_paradata_full %>% get_pooled_estiamte(),
  #           by = "term") %>% 
  #   left_join(
  #     left_join(x = model_time3_demographic_full %>% get_pooled_estiamte(),
  #               y = model_time2_demographic_full %>% get_pooled_estiamte(),
  #               by = "term"),
  #     by = "term"
  #   ) %>% 
    # rename_at(vars(ends_with(".x.x")), 
    #           ~ str_replace(., ".x.x", "_m2_time3")) %>% 
    # rename_at(vars(ends_with(".y.x")), 
    #           ~ str_replace(., ".y.x", "_m2_time2")) %>% 
    # rename_at(vars(ends_with(".x.y")), 
    #           ~ str_replace(., ".x.y", "_m1_time3")) %>% 
    # rename_at(vars(ends_with(".y.y")), 
    #           ~ str_replace(., ".y.y", "_m1_time2")) %>% 
  #   select(term, 
  #          ends_with("_m1_time2"), ends_with("_m1_time3"),
  #          ends_with("_m2_time2"), ends_with("_m2_time3"),
  #          -starts_with("p.value")) %>% 
  # mutate(term = covariate_label_full)


# ========== 2.6 format model output table ==========
table_model_1_and_2 <-
  table_model_1_and_2 %>%
    mutate(across(starts_with("estimate_"), ~ format(., digits = 0, nsmall = 2))) %>%
    unite(col = "estimate_m1_time2", dplyr::ends_with("m1_time2"), sep = "") %>% 
    # unite(col = "estimate_m1_time3", dplyr::ends_with("m1_time3"), sep = "") %>% 
    unite(col = "estimate_m2_time2", dplyr::ends_with("m2_time2"), sep = "") %>% 
    # unite(col = "estimate_m2_time3", dplyr::ends_with("m2_time3"), sep = "") %>% 
    add_row(table_gof_model_1_and_2) %>%
    mutate(across(.cols = starts_with("estimate"), 
                  .fns = function(.x) case_when(.x %in% c("  NANA", NA) ~ "", TRUE ~ .x))) %>% 
    kable(col.names = c("Variable", 
                        "Model 1",
                        # "Time3 + demographics",
                        "Model 2")) %>% 
                        # "Time3 + demographics + paradata")) %>% 
    pack_rows(group_label = "Education (ref: high school or below)",
              start_row = str_which(table_model_1_and_2$term, "College"),
              end_row = str_which(table_model_1_and_2$term, "Bachelor or above"),
              bold = F,
              label_row_css = "border-bottom: 0px;") %>%
    pack_rows(group_label = "Household income (ref: low)",
              start_row = str_which(table_model_1_and_2$term, "Middle"),
              end_row = str_which(table_model_1_and_2$term, "High"),
              bold = F,
              label_row_css = "border-bottom: 0px;") %>%
    pack_rows(group_label = "Topic (ref: commitment statement)",
              start_row = str_which(table_model_1_and_2$term, "Demographics"),
              end_row = str_which(table_model_1_and_2$term, "Income"),
              bold = F,
              label_row_css = "border-bottom: 0px;") %>%
    kable_styling() %>% 
    add_footnote("* p < 0.1, ** p < 0.05, *** p < 0.01", notation = "none")
  


table_model_1_and_2




# ========== 3. report model 3 & 4 ===========

# ========== 3.1 create covariate label for model output table ===========
# check the original model covariate labels
model_time2_demographic_paradata_interaction_restricted$time2_impute_1 %>% 
  tidy(.) %>% 
  print(n = nrow(.)) %>% 
  pull(term)

# create new model covariate labels
covariate_label_restricted  <-
  c(
    str_subset(covariate_label_full,
               "Demographics|Housing|Clothing|Income|Matrix", 
                negate = T),
    "Grouped x Questions seen",
    "Grouped x Questions seen2",
    # "Grouped x Questions seen3",
    "Utilities x Questions seen",
    "Insurance x Questions seen",
    "Utilities x Questions seen2",
    "Insurance x Questions seen2"
    # "Utilities x Questions seen3",
    # "Insurance x Questions seen3"
  )


# check the re-coding of variable names
tibble(model_time2_demographic_paradata_interaction_restricted$time2_impute_1 %>% 
         tidy(.) %>% 
         pull(term),
       covariate_label_restricted) %>% 
  View()


# ========== 3.2 get the number of respondents in model ===========
n_respondent_model_restricted <-
  data_for_model_restricted %>%
  map( ~ .x %>% distinct(case_id)) %>% 
  map_dbl( ~ .x %>% nrow()) %>% 
  unique()


# ========== 3.3 get model output table ===========
table_model_3_and_4 <-
  left_join(x = model_time2_demographic_paradata_interaction_restricted %>% get_pooled_estiamte(),
            y = model_time2_demographic_paradata_restricted %>% get_pooled_estiamte(),
            by = "term") %>% 
  rename_at(vars(ends_with(".x")), 
            ~ str_replace(., ".x", "_m4_time2")) %>% 
  rename_at(vars(ends_with(".y")), 
            ~ str_replace(., ".y", "_m3_time2")) %>% 
  select(term, 
         ends_with("_m3_time2"), ends_with("_m4_time2"),
         -starts_with("p.value")) %>% 
  mutate(term = covariate_label_restricted)
  # left_join(x = model_time3_demographic_paradata_interaction_restricted %>% get_pooled_estiamte(),
  #           y = model_time2_demographic_paradata_interaction_restricted %>% get_pooled_estiamte(),
  #           by = "term") %>% 
  # left_join(
  #   left_join(x = model_time3_demographic_paradata_restricted %>% get_pooled_estiamte(),
  #             y = model_time2_demographic_paradata_restricted %>% get_pooled_estiamte(),
  #             by = "term"),
  #   by = "term"
  # ) %>% 
  # rename_at(vars(ends_with(".x.x")), 
  #           ~ str_replace(., ".x.x", "_m4_time3")) %>% 
  # rename_at(vars(ends_with(".y.x")), 
  #           ~ str_replace(., ".y.x", "_m4_time2")) %>% 
  # rename_at(vars(ends_with(".x.y")), 
  #           ~ str_replace(., ".x.y", "_m3_time3")) %>% 
  # rename_at(vars(ends_with(".y.y")), 
  #           ~ str_replace(., ".y.y", "_m3_time2")) %>% 
  # select(term, 
  #        ends_with("_m3_time2"), ends_with("_m3_time3"),
  #        ends_with("_m4_time2"), ends_with("_m4_time3"),
  #        -starts_with("p.value")) %>% 
  # mutate(term = covariate_label_restricted)
  

# ========== 3.4 change the order of interaction terms ===========
# check the original order of the model variables
table_model_3_and_4 %>% View()

# put together interaction terms of the same type
table_model_3_and_4 <- 
  table_model_3_and_4 %>% 
  slice(1:str_which(.$term, "Grouped x Questions seen2")) %>% 
  bind_rows(table_model_3_and_4 %>% 
              filter(str_detect(.$term, "Utilities x"))) %>% 
  bind_rows(table_model_3_and_4 %>% 
              filter(str_detect(.$term, "Insurance x"))) 

# check the re-ordering result
table_model_3_and_4 %>% View()


# ========== 3.5 get the goodness of fit of model ===========
table_gof_model_3_and_4 <-
  map(list(model_time2_demographic_paradata_restricted,
           # model_time3_demographic_paradata_restricted,
           model_time2_demographic_paradata_interaction_restricted),
           # model_time3_demographic_paradata_restricted),
      ~ get_model_gof(.list_of_model = ., .n_respondent = n_respondent_model_restricted)) %>% 
  reduce(left_join, by = "Variable") %>% 
  rename_all(~ c("term", "estimate_m3_time2", "estimate_m4_time2"))


# ========== 3.5 format model output table ===========
table_model_3_and_4 <-
  table_model_3_and_4 %>%
  mutate(across(starts_with("estimate_"), ~ format(., digits = 0, nsmall = 2))) %>%
  unite(col = "estimate_m3_time2", dplyr::ends_with("m3_time2"), sep = "") %>% 
  # unite(col = "estimate_m3_time3", dplyr::ends_with("m3_time3"), sep = "") %>% 
  unite(col = "estimate_m4_time2", dplyr::ends_with("m4_time2"), sep = "") %>% 
  # unite(col = "estimate_m4_time3", dplyr::ends_with("m4_time3"), sep = "") %>% 
  add_row(table_gof_model_3_and_4) %>%
  mutate(across(.cols = starts_with("estimate"), 
                .fns = function(.x) case_when(.x %in% c("  NANA", NA) ~ "", TRUE ~ .x))) %>% 
  kable(col.names = c("Variable", 
                      "Model 3",
                      # "Time3 + demographics + paradata",
                      "Model 4")) %>% 
                      # "Time3 + demographics + paradata + interaction")) %>% 
  pack_rows(group_label = "Education (ref: high school or below)",
            start_row = str_which(table_model_3_and_4$term, "College"),
            end_row = str_which(table_model_3_and_4$term, "Bachelor or above"),
            bold = F,
            label_row_css = "border-bottom: 0px;") %>%
  pack_rows(group_label = "Household income (ref: low)",
            start_row = str_which(table_model_3_and_4$term, "Middle"),
            end_row = str_which(table_model_3_and_4$term, "High"),
            bold = F,
            label_row_css = "border-bottom: 0px;") %>%
  pack_rows(group_label = "Topic (ref: commitment statement)",
            start_row = str_which(table_model_3_and_4$term, "Utilities$"),
            end_row = str_which(table_model_3_and_4$term, "Insurance$"),
            bold = F,
            label_row_css = "border-bottom: 0px;") %>%
  kable_styling() %>% 
  add_footnote("* p < 0.1, ** p < 0.05, *** p < 0.01", notation = "none")



table_model_3_and_4




# # ========== 4. report model - time only ===========
# 
# # ========== 4.1 check names of the term/variable in model ===========
# model_time_polynomial %>% 
#   map( ~ tidy(.x)) %>% 
#   walk( ~ print(.x, n = nrow(.x)))
# 
# 
# # ========== 4.2 create covariate label for model output table ===========
# covariate_label_time_only  <-
#   c(
#     "poly(question_sequence, .x, raw = TRUE)" = "Number of questions seen",
#     "poly(question_sequence, .x, raw = TRUE)1" = "Number of questions seen",
#     "poly(question_sequence, .x, raw = TRUE)2" = "Number of questions seen2",
#     "poly(question_sequence, .x, raw = TRUE)3" = "Number of questions seen3",
#     "poly(question_sequence, .x, raw = TRUE)4" = "Number of questions seen4",
#     "poly(question_sequence, .x, raw = TRUE)5" = "Number of questions seen5"
#   )
# 
# 
# # ========== 4.3 get the number of respondents in model ===========
# n_respondent_model_time_only <- 
#   data_for_model %>%
#   map( ~ .x %>% distinct(case_id)) %>% 
#   map_dbl( ~ .x %>% nrow()) %>% 
#   unique() %>% 
#   format(big.mark = ",")
# 
# 
# # ========== 4.4 create model output table ===========
# table_model_time_polynomial <-   
#   modelsummary(list("Linear time" = model_time_polynomial$time1,
#                     "Squared time" = model_time_polynomial$time2,
#                     "Cubic time" = model_time_polynomial$time3,
#                     "4th" = model_time_polynomial$time4,
#                     "5th" = model_time_polynomial$time5),
#                output = "gt",
#                fmt = "%.2f",
#                exponentiate = T, 
#                stars = T,
#               statistic_vertical = F,
#               coef_map = covariate_label_time_only) %>% 
#   as.data.frame() %>% 
#   as_tibble() %>% 
#   rename("Variable" = " ") %>%
#   mutate(Variable = case_when(Variable == "Num.Obs." ~ "Observations",
#                               Variable == "Log.Lik." ~ "Log Likelihood",
#                               TRUE ~ Variable)) %>% 
#   add_row(Variable = "Respondents",
#           .before = str_which(.$Variable, "Observations")) %>% 
#   print(n = nrow(.)) %>%
#   gt() %>% 
#   text_transform(locations = cells_body(columns = 2:6,
#                                         rows = Variable == "Respondents"),
#                  fn = function(.x) n_respondent_model_time_only) %>% 
#   apply_table_style(.column_end = 6)
# 
# 
# table_model_time_polynomial
# 
# 
# # ========== 4.5 create model goodness-of-fit comparison table ===========
# table_gof_comparison <- 
#   map_df(model_time_polynomial, 
#          ~ glance(.x) %>% 
#            select(null.deviance, deviance, aic= AIC, bic = BIC) %>% 
#            mutate(n = nobs(.x)), 
#          .id = "model") %>% 
#   # columns "null.deviance" and "n" are the same for all models --> remove these two columns
#   select(-null.deviance, -n) %>%
#   # calculate the change in the model fit
#   mutate(deviance_diff = lag(deviance) - deviance,
#          aic_diff = lag(aic) - aic,
#          bic_diff = lag(bic) - bic) %>% 
#   mutate(model = c("Linear time", "Squared time", "Cubic time", "4th", "5th")) %>% 
#   gt() %>% 
#   cols_label(
#     "model" = "Time specification",
#     "deviance" = "Deviance",
#     "aic" = "AIC",
#     "bic" = "BIC",
#     "deviance_diff" = "Deviance difference",
#     "aic_diff" = "AIC difference",
#     "bic_diff" = "BIC difference"
#   ) %>% 
#   fmt_missing(columns = everything(),
#               missing_text = "-") %>% 
#   fmt_number(columns = 2:7,
#              decimals = 0) %>% 
#   tab_style(locations = cells_column_labels(everything()),
#             style = cell_text(weight = "bold")) %>% 
#   tab_options(column_labels.border.top.color = "black",
#               column_labels.border.bottom.color = "black",
#               table_body.border.bottom.color = "black",
#               table_body.hlines.color = "white")
# 
# 
# table_gof_comparison





# ========== 5. export model output table ===========
walk2(
  .x = list(table_model_1_and_2,
            table_model_3_and_4),
  .y = list("table-model-1-and-2",
            "table-model-3-and-4"),
  ~ save_kable(.x,
               file = str_c(rprojroot::find_rstudio_root_file(),"/doc/", .y, ".html"))
)



# walk2(
#   .x = list(table_model_time_polynomial,
#             table_gof_comparison),
#   .y = list("table-model-time-polynomial",
#             "table-gof-comparison"),
#   ~ gtsave(.x,
#            filename = str_c(.y, ".html"),
#            path = str_c(rprojroot::find_rstudio_root_file(),"/doc"))
# )

