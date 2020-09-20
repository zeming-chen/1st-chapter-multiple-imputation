library(tidyverse)


clean_de_blocks <- function(.data) {
  # d25
  if (!.data[.data$question_name == "d25", "answer"] %in% "Yes") {
    .data[.data$question_name == "d28", "question_sequence"] <-
      NA
    
    .data[(which(.data$question_name == "d28") + 1):nrow(.data), "question_sequence"] <-
      .data[(which(.data$question_name == "d28") + 1):nrow(.data), "question_sequence"] - 1
    
  }
  
  # e2
  if (!.data[.data$question_name == "e2", "answer"] %in% "Other - Specify") {
    .data[.data$question_name == "e2o", "question_sequence"] <-
      NA
    
    .data[(which(.data$question_name == "e2o") + 1):nrow(.data), "question_sequence"] <-
      .data[(which(.data$question_name == "e2o") + 1):nrow(.data), "question_sequence"] - 1
    
  }
  
  # e9, e19, e21_1, e21_2
  if (.data[.data$question_name == "e9", "answer"] %in% "Yes") {
    .data[.data$question_name == "e19", "question_sequence"] <-
      NA
    .data[.data$question_name == "e21_1", "question_sequence"] <-
      NA
    
    .data[(which(.data$question_name == "e21_1") + 1):nrow(.data), "question_sequence"] <-
      .data[(which(.data$question_name == "e21_1") + 1):nrow(.data), "question_sequence"] - 2
    
  } else if (!.data[.data$question_name == "e9", "answer"] %in% "Yes" &
             .data[.data$question_name == "e19", "answer"] %in% "No") {
    .data[.data$question_name == "e21_1", "question_sequence"] <-
      NA
    .data[.data$question_name == "e21_2", "question_sequence"] <-
      NA
    
    .data[.data$question_name == "ceintro", "question_sequence"] <-
      .data[.data$question_name == "ceintro", "question_sequence"] - 2
    
  } else if (!.data[.data$question_name == "e9", "answer"] %in% "Yes" &
             !.data[.data$question_name == "e19", "answer"] %in% "No") {
    .data[.data$question_name == "e21_2", "question_sequence"] <-
      NA
    
    .data[.data$question_name == "ceintro", "question_sequence"] <-
      .data[.data$question_name == "ceintro", "question_sequence"] - 1
  } # close bracket for else if(){} 
  .data
} # close bracket for the function(){}
