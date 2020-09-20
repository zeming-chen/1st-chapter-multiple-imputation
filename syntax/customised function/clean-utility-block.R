library(tidyverse)


# create a table for the function below to loop over
lookup_table_utility_block <-
  rbind(paste0("b", 1:5, "_2"),
        paste0("b", 1:5, "_5o"),
        paste0("b", 1:5, "_5"),
        paste0("b", 1:5, "_5o")) %>%
  as_tibble() %>%
  set_names(paste0("filter", 7:11))


clean_utility_block <- function(.data) {
  
  for (i in paste0("filter", 7:11)) {
    
    if (!.data[.data$question_name == i, "answer"] %in% "Yes") {
      .data[which(.data$question_name == lookup_table_utility_block[[1, i]]):which(.data$question_name == lookup_table_utility_block[[2, i]]),
            "question_sequence"] <- NA
      
      .data[(which(.data$question_name == lookup_table_utility_block[[2, i]]) +
               1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_utility_block[[2, i]]) +
                 1):nrow(.data),
              "question_sequence"] - 5
      
      
    } else if (!.data[.data$question_name == lookup_table_utility_block[[3, i]], "answer"] %in% "Other") {
      .data[.data$question_name == lookup_table_utility_block[[4, i]], "question_sequence"] <-
        NA
      
      .data[(which(.data$question_name == lookup_table_utility_block[[4, i]]) +
               1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_utility_block[[4, i]]) +
                 1):nrow(.data),
              "question_sequence"] - 1
    } # close bracket for else if{}
  } # close bracket for for(){}
  .data
} # close bracket for function(){}