library(tidyverse)

# function to work out the question sequence for each a, b, and c block order
derive_question_sequence <- function(.input_to_map_over) {
  
  # split a length-one vector of question block names into a length-three vector
  # "cloth, utility, insurance" to "cloth", "utility", "insurance"
  # then, transform this length-three vector to a 1x3 matrix
  # doing this will help figure out what is the first/second/third question block the respondent is assigned to
  matrix_question_block_name <- 
    str_split(.input_to_map_over, pattern = ", ", simplify = TRUE)
  
  # get the total number of questions in the 1st question block using 4 steps:
  # STEP 1: identify the name of the FIRST question block for the respondent using "matrix_question_block_name[[1]]"
  # STEP 2: extract the total number of questions in this FIRST question block using "vector_total_questions_in_each_abc_question_block[STEP 1 result]"
  # STEP 3: create the question sequence starting from 1 to the total number of questions in the FIRST question block using "1:STEP 2 result"
  # STEP 4: attach the name of the FIRST question block for later data manipulation
  first_question_block_length <- 
    data.frame(
      question_sequence = 1:vector_total_questions_in_each_abc_question_block[matrix_question_block_name[[1]]],
      block_name = matrix_question_block_name[[1]]
    ) 
  
  # get the total number of questions in the 2nd question block using 4 steps:
  # STEP 1: identify the name of the SECOND question block for the respondent using "matrix_question_block_name[[2]]"
  # STEP 2: extract the total number of questions in this SECOND question block using "vector_total_questions_in_each_abc_question_block[STEP 1 result]"
  # STEP 3: create the question sequence for the SECOND question block. It starts right after the end of the first question block's sequence (i.e. "nrow(first_question_block_length) + 1"). It ends at the total number of questions in the SECOND question block (i.e. "vector_total_questions_in_each_abc_question_block[matrix_question_block_name[[2]]]") PLUS the total number of questions in the FIRST question block (i.e. "nrow(first_question_block_length).
  # STEP 4: attach the name of the SECOND question block to help data manipulation later
  second_question_block_length <- 
    data.frame(
      question_sequence = (nrow(first_question_block_length) + 1):(nrow(first_question_block_length) + vector_total_questions_in_each_abc_question_block[matrix_question_block_name[[2]]]),
      block_name = matrix_question_block_name[[2]]
    )
  
  # get the total number of questions in the 3rd question block using the logic:
  third_question_block_length <- 
    data.frame(
      question_sequence = (nrow(first_question_block_length) + nrow(second_question_block_length) + 1):(nrow(first_question_block_length) + nrow(second_question_block_length) + vector_total_questions_in_each_abc_question_block[matrix_question_block_name[[3]]]),
      block_name = matrix_question_block_name[[3]]
    )
  
  # combine the question sequence of all three question blocks
  rbind(first_question_block_length,
        second_question_block_length,
        third_question_block_length) %>%
    as_tibble() %>%
    # order rows of the data frame
    arrange(match(block_name, c("cloth", "utility", "insurance"))) %>%
    # remove the un-needed column once the ordering is done
    select(-block_name) %>%
    # rename the columns according to the function input
    set_names(.input_to_map_over)
} # close bracket for function(){}