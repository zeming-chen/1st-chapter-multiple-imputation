library(tidyverse)


# create a table for the function below to loop through
lookup_table_income <-
  rbind(c("f8", "f26", "f30", "f46", "f48"),
        c("f10", "f27", "f31", "f47", "f49")) %>%
  as_tibble() %>%
  set_names(c("f8", "f26", "f30", "f46", "f48"))


clean_f_blocks <- function(.data){
  
  # clean "f8", "f26", "f30", "f46", "f48" and their corresponding pairs
  for (k in c("f8", "f26", "f30", "f46", "f48")) {
    
    if (.data[.data$question_name == lookup_table_income[[1, k]], "answer"] %in% "No") {
      .data[.data$question_name == lookup_table_income[[2, k]], "question_sequence"] <-
        NA
      
      .data[which(.data$question_name == lookup_table_income[[2, k]]):nrow(.data), "question_sequence"] <-
        .data[which(.data$question_name == lookup_table_income[[2, k]]):nrow(.data), "question_sequence"] -
        1
    } # close bracket for the if statement
  } # close bracket for the for loop
  
  # clean f24 and f25 pair
  if (.data[.data$question_name == "f24", "answer"] %in% c("No", "Self-employment resulted in a loss")) {
    
    .data[.data$question_name == "f25", "question_sequence"] <- NA
    
    .data[(which(.data$question_name == "f25")+1):nrow(.data), "question_sequence"] <-
      .data[(which(.data$question_name == "f25")+1):nrow(.data), "question_sequence"] -
      1
  }
  
  # clean f32 and f33 pair
  if (.data[.data$question_name == "f32", "answer"] %in% c("No", "Rental resulted in a loss")) {
    
    .data[.data$question_name == "f33", "question_sequence"] <- NA
    
    .data[(which(.data$question_name == "f33")+1):nrow(.data), "question_sequence"] <-
      .data[(which(.data$question_name == "f33")+1):nrow(.data), "question_sequence"] -
      1
  } # close bracket for if(){}
  .data
} # close bracket for the function(){}
