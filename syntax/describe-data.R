# Table of Content
#
# 1. prepare work space
#
# 2. describe data - numeric
#   2.1 create descriptive table
#   2.2 format descriptive table
#
# 3. describe data - categorical - respondent level
#   3.1 create descriptive table
#   3.2 format descriptive table
#
# 4. describe data - categorical - question level
#   4.1 create descriptive table
#   4.2 format descriptive table
#
# 5. plot kaplan meier curve
#   5.1 plot breakoff hazard vs. question sequence - all respondents
#   5.2 plot survival function vs. time by filter formats - restricted dataset
#
# 6. export table and plot




# ====== 1. prepare work space ======
library(tidyverse)   # for manipulating data
library(survival)    # for running survival model
library(survminer)   # for visualising survival model
library(gt)          # for formatting table output

getwd()

load("./data/derived/data-for-analysis.RData")

source("./syntax/customised function/describe-data-numeric.R")




# ====== 2. describe data - numeric ======

# ====== 2.1 create descriptive table ======
table_descriptive_numeric <-
  data_for_descriptive_analysis %>% 
  # create descriptive summary for time-constant numeric variables
  select_if(is.numeric) %>% 
  select(-case_id) %>% 
  map_df( ~ describe_data_numeric(.x)) %>% 
  # append descriptive summary for two time-varying numeric variables:
  # variable 1 appended: word count in the question stem
  add_row(data_meta %>% select(word_count) %>% map_df(~ describe_data_numeric(.x))) %>% 
  # variable 2 appended: item nonresponse rate
  add_row(data_long %>% 
            group_by(case_id) %>% 
            summarise(item_nonresponse_rate_max = max(item_missing_percent)) %>% 
            pull(item_nonresponse_rate_max) %>%
            describe_data_numeric()
          ) %>% 
  select(-na_n) %>% 
  # round all numeric columns to the 2nd decimal
  mutate_all(~ round(., 2)) %>% 
  # add a column for the variable name
  add_column(variable = c(data_for_descriptive_analysis %>% select_if(is.numeric) %>% select(-case_id) %>% colnames(),
                          data_meta %>% select(word_count) %>% colnames(),
                          data_long %>% select(item_missing_percent) %>% colnames()),
             .before = 1)


# ====== 2.2 format descriptive table ======
table_descriptive_numeric <- 
  table_descriptive_numeric %>%
  # format the value in the column "variable" publishable
  mutate(variable = c("Number of questions seen",
                      "Survey duration (min)", 
                      "Age", 
                      "Word count of question stems",
                      "Item nonresponse rate")) %>%
  # add two rows to distinguish time-varying/constant variables
  add_row(variable = "Respondent level (N = 3128)", .before = 1) %>% 
  add_row(variable = "Question level (N = 196)", .before = 4) %>% 
  gt() %>% 
  # rename column
  cols_label(
    variable = "Variable",
    min = "Min",
    max = "Max",
    median = "Median",
    mean = "Mean",
    sd = "SD",
    n = "N",
    na_prop = "Missing (%)"
  ) %>% 
  # replace NA values with blank
  fmt_missing(columns = everything(), missing_text = "") %>% 
  # format the table
  tab_style(locations = cells_column_labels(everything()),
            style = cell_text(weight = "bold")) %>% 
  tab_style(locations = cells_body(columns = "variable",
                                   rows = variable %in% c("Respondent level (N = 3128)", "Question level (N = 196)")),
            style = cell_text(style = "italic")) %>% 
  tab_style(locations = cells_body(columns = everything()),
            style = cell_text(align = "left")) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white")
  

table_descriptive_numeric


  

# ====== 3. describe data - categorical - respondent level ======

# ====== 3.1 create descriptive table ======
table_descriptive_categorical_respondent_level <-
  data_for_descriptive_analysis %>%
  select_if(is.factor) %>%
  map_df( ~ table(.x, useNA = "always") %>% as_tibble(),
          .id = "variable") %>% 
  rename(category = ".x") %>% 
  group_by(variable) %>% 
  mutate(prop = n / sum(n) * 100) %>% 
  ungroup() %>% 
  # remove the NA category if it does not exist in the variable
  filter(!(category %in% NA & n == 0)) %>% 
  # round the column of percentage to the second decimal point
  mutate(prop = round(prop, 2)) %>% 
  mutate(category = ifelse(category %in% NA, "Missing", category)) %>% 
  mutate(variable = str_remove(variable, "_recode")) %>%
  mutate(variable = case_when(variable == "grouped" ~ "Filer question format",
                              variable == "number_of_connection" ~ "Number of sessions",
                              variable == "device" ~ "Responding device",
                              variable == "married" ~ "Marital status",
                              variable == "in_school" ~ "Current student",
                              variable == "educ" ~ "Education",
                              variable == "household_income" ~ "Household income",
                              TRUE ~ variable)) %>%
  mutate(variable = str_to_sentence(variable))


table_descriptive_categorical_respondent_level %>% print(n = nrow(.))


# ====== 3.2 format descriptive table ======
table_descriptive_categorical_respondent_level <- 
  table_descriptive_categorical_respondent_level %>%
  gt(groupname_col = "variable",
     rowname_col = "category") %>% 
  # rename column
  cols_label(
    n = "Frequency",
    prop = "Percent (%)"
  ) %>% 
  # add the column name for the stubhead (i.e. 1st column)
  tab_stubhead(label = "Variable") %>% 
  # indent the category of the variable
  tab_style(locations = cells_stub(),
            style = cell_text(indent = px(10))) %>% 
  # format the table
  tab_style(locations = cells_row_groups(),
            style = cell_text(weight = "bold")) %>%
  tab_style(locations = cells_column_labels(everything()),
            style = cell_text(weight = "bold")) %>% 
  tab_style(locations = cells_stubhead(),
            style = cell_text(weight = "bold")) %>%
  tab_options(column_labels.border.top.color = "black",
              row_group.border.top.color = "white",
              row_group.border.bottom.color = "white",
              stub.border.color = "white",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") 


table_descriptive_categorical_respondent_level




# ===== 4. describe data - categorical - question level =====

# ====== 4.1 create descriptive table ======
table_descriptive_categorical_question_level <-
  data_meta %>%
  select_if(is.factor) %>%
  map_df( ~ table(.x, useNA = "always") %>% as_tibble(),
          .id = "variable") %>% 
  rename(category = ".x") %>% 
  group_by(variable) %>% 
  mutate(prop = n / sum(n) * 100) %>% 
  ungroup() %>% 
  # remove the NA category if it does not exist in the variable
  filter(!(category %in% NA & n == 0)) %>% 
  # round the column of percentage to the second decimal point
  mutate(prop = round(prop, 2)) %>% 
  # change the value in the column of "variable"
  mutate(variable = case_when(variable == "open_ended" ~ "Open-ended questions",
                              variable == "intro_page" ~ "Introduction statement",
                              variable == "topic" ~ "Topic",
                              variable == "matrix" ~ "Matrix question")) %>% 
  # capitalise the first letter of the values in the column "category"
  mutate(category = str_to_sentence(category))

  
table_descriptive_categorical_question_level %>% print(n = nrow(.))
  

# ====== 4.2 format descriptive table ======
table_descriptive_categorical_question_level <- 
  table_descriptive_categorical_question_level %>%
  gt(groupname_col = "variable",
     rowname_col = "category") %>% 
  # rename column
  cols_label(
    n = "Frequency",
    prop = "Percent (%)"
  ) %>% 
  # add the column name for the stubhead (i.e. 1st column)
  tab_stubhead(label = "Variable") %>% 
  # indent the category of the variable
  tab_style(locations = cells_stub(),
            style = cell_text(indent = px(10))) %>% 
  # format the table
  tab_style(locations = cells_row_groups(),
            style = cell_text(weight = "bold")) %>%
  tab_style(locations = cells_column_labels(everything()),
            style = cell_text(weight = "bold")) %>% 
  tab_style(locations = cells_stubhead(),
            style = cell_text(weight = "bold")) %>%
  tab_options(column_labels.border.top.color = "black",
              row_group.border.top.color = "white",
              row_group.border.bottom.color = "white",
              stub.border.color = "white",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") 
  

table_descriptive_categorical_question_level




# ====== 5. plot kaplan meier curve ======

# ===== 5.1 plot breakoff hazard vs. question sequence - all respondents =====
plot_hazard_time <- 
  survfit(Surv(question_sequence, breakoff_status) ~ 1, 
        data = data_for_km_plot) %>% 
  surv_summary() %>% 
  as_tibble() %>% 
  mutate(hazard = n.event / n.risk) %>% 
  ggplot(aes(x = time, y = hazard)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 0, to = 190, by = 10),
                     expand = c(0, 2)) + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic() +
  labs(x = "Number of questions seen", y = "Hazard h(t)") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


plot_hazard_time


# ===== 5.2 plot survival function vs. time by filter formats - restricted dataset =====
plot_survival_time_by_filter_format <- 
  survfit(Surv(question_sequence, breakoff_status) ~ filter_format, 
          data = filter(data_for_km_plot, 
                        str_detect(last_question_filled, "^(q|d|e[0-9])|ceintro", negate = TRUE))) %>%
  ggsurvplot(ylim = c(0.85, 1), xlim = c(18, 195), 
             break.time.by = 10,
             axes.offset = FALSE,
             palette = "grey",
             legend.title = "Filter question format",
             legend.labs = c("Grouped", "Interleafed"),
             ylab = "Survivor probability S(t)",
             xlab = "Number of questions seen") %>% 
  ggpar(font.legend = c(15), font.x = c(15), font.y = c(15))


plot_survival_time_by_filter_format




# ====== 6. export table and plot ======
walk2(
    .x = list(table_descriptive_numeric, 
              table_descriptive_categorical_respondent_level,
              table_descriptive_categorical_question_level),
    .y = list("table-descriptive-numeric",
              "table-descriptive-categorical-respondent-level",
              "table-descriptive-categorical-question-level"),
    ~ gtsave(.x,
             filename = str_c(.y, ".html"),
             path = str_c(rprojroot::find_rstudio_root_file(),"/doc"))
    )


ggsave(plot = plot_hazard_time,
       filename = "plot_hazard_vs_time.png",
       path = "./plot",
       width = 18, height = 10, units = "cm")

ggsave(plot = plot_survival_time_by_filter_format$plot,
       filename = "plot_survival_time_by_filter_format.png",
       path = "./plot",
       width = 18, height = 10, units = "cm")


