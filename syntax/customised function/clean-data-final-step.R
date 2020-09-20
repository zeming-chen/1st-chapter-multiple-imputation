library(tidyverse)


# create a function to remove rows (i.e. questions) that the respondents did not answer
clean_data_final_step <- 
  function(.data_with_a1b1c1, .data_with_info_of_last_question_filled) {
  
  # add the last question filled for each respondent
  temporary_df <-
    left_join(x = .data_with_a1b1c1,
              y = .data_with_info_of_last_question_filled %>% select(nid, last_question_filled),
              by = c("case_id" = "nid"))
  
  last_question_filled <- 
    temporary_df %>%
    pull(last_question_filled) %>%
    unique()
  
  position_to_start_to_remove_rows <-
    which(temporary_df$question_name == last_question_filled) + 1
  
  # the below if-statement does 3 things:
  # 1) remove the rows that are after the row corresponding to the last question filled
  # 2) create a censor variable for the outcome of interest (called "breakoff_status")
  # 3) create the discrete time interval (i.e. "start" and "end")
  if (last_question_filled == "ending") {
    temporary_df <- 
      temporary_df %>%
      filter(!is.na(question_sequence)) %>%
      mutate(
        breakoff_status = 0,
        start = 0:(nrow(.) - 1),
        end = start + 1) %>% 
      mutate(end = as.integer(end))
    
  } else {
    
    temporary_df <- 
      temporary_df %>%
      slice(-position_to_start_to_remove_rows:-nrow(temporary_df)) %>%
      filter(!is.na(question_sequence)) %>%
      mutate(start = 0:(nrow(.) - 1), 
             end = start + 1) %>% 
      mutate(end = as.integer(end))
    
    temporary_df$breakoff_status <- c(rep(0, (nrow(temporary_df) - 1)), 1)
    
  } # close bracket for else{}
  temporary_df
} # close bracket for function(){}
