library(tidyverse)


# create a lookup table for replacing the old last question filled
# this table will be used inside the function below
lookup_table_for_last_questions_filled <-
  c(paste0("filter", 1:16),
    paste0("faddl", 1:6),
    paste0("faddl", 12:15),
    paste0("price", 12:16)) %>% 
  set_names(
    c(paste0("a", 1:6, "_1"),
      paste0("b", 1:5, "_1"),
      paste0("c", 1:5, "_1"),
      paste0("a", 1:6, "_11"),
      paste0("c", 1:4, "_11"),
      paste0("c", 1:5, "_5")))


recode_last_question_filled  <-
  function(.old_last_question_filled) {
    # if respondents' last answered question has this pattern: 
    # a letter followed by a number, an underscore and then one/two numbers (i.e. "a|b|c[1-5]_[1]+")
    # replace it with the new question name defined in the lookup table above
    if (.old_last_question_filled %in% names(lookup_table_for_last_questions_filled)) {
      .new_last_question_filled <- lookup_table_for_last_questions_filled[.old_last_question_filled]
    } else if (.old_last_question_filled %in% NA) {
    # if the last question respondents answered is missing
    # it means respondents broke off at the first question
      .new_last_question_filled <- "q1"
    } else {
    # for other values of last questions filled
    # copy over the original value
      .new_last_question_filled <- .old_last_question_filled
    } # close bracket for else{}
    
  } # close bracket for function()