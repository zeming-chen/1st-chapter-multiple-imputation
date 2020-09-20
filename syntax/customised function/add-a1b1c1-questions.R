library(tidyverse)


add_a1b1c1_as_new_rows <- function(.data) {
  # insert "a1", "b1", "c1" as new rows right before the start of its blocks
  temporary_df <- 
    .data %>%
    add_row(question_name = "a1",
            .before = str_which(.data$question_name, "filter1$")) %>%
    add_row(question_name = "b1",
            .before = str_which(.data$question_name, "filter7")) %>%
    add_row(question_name = "c1",
            .before = str_which(.data$question_name, "filter12"))
  
  # derive the starting position of the abc randomised question blocks
  start_position_of_cloth_block <-
    str_which(temporary_df$question_name, "filter1$")
  
  start_position_of_utility_block <-
    str_which(temporary_df$question_name, "filter7")
  
  start_position_of_insurance_block <-
    str_which(temporary_df$question_name, "filter12")
  
  # sort the starting position of the abc randomised question blocks
  start_position_of_abc_question_blocks_sorted <- 
    sort(c(start_position_of_cloth_block,
           start_position_of_utility_block,
           start_position_of_insurance_block))
  
  # change the question sequence after adding in "a1", "b1", "c1"
  for (i in 1:3) {
    temporary_df[start_position_of_abc_question_blocks_sorted[i]:nrow(temporary_df), "question_sequence"] <-
      temporary_df[start_position_of_abc_question_blocks_sorted[i]:nrow(temporary_df), "question_sequence"] + 1
  }
  
  # assign question sequence for "a1", "b1", "c1"
  for (j in 1:3) {
    temporary_df[str_which(temporary_df$question_name, "[a-c]1$")[j], "question_sequence"] <-
      temporary_df[1:(str_which(temporary_df$question_name, "[a-c]1$")[j]), "question_sequence"] %>% max(na.rm = TRUE) + 1
  }
  
  # create two columns "case_id" and "filter_format"
  temporary_df$case_id <- .data$case_id %>% unique()
  temporary_df$filter_format <- .data$filter_format %>% unique() %>% na.omit()
  
  temporary_df
} # close bracket for function()