library(tidyverse)


# create a table for the function below to loop over 
lookup_table_cloth_block <-
  rbind(paste0("a", 1:6, "_1desc"),
        paste0("a", 1:6, "_15"),
        paste0("faddl", 1:6),
        paste0("a", 1:6, "_11desc"),
        paste0("a", 1:6, "_15")) %>%
  as_tibble() %>%
  set_names(paste0("filter", 1:6))


clean_cloth_block <- function(.data) {
  
  for (i in paste0("filter", 1:6)) {
    
    if (!.data[.data$question_name == i, "answer"] %in% "Yes") {
      .data[which(.data$question_name == lookup_table_cloth_block[[1, i]]):which(.data$question_name == lookup_table_cloth_block[[2, i]]),
            "question_sequence"] <- NA
      
      .data[(which(.data$question_name == lookup_table_cloth_block[[2, i]]) + 1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_cloth_block[[2, i]]) + 1):nrow(.data),
              "question_sequence"] - 11
      
    } else if (!.data[.data$question_name == lookup_table_cloth_block[[3, i]], "answer"] %in% "Yes") {
      .data[which(.data$question_name == lookup_table_cloth_block[[4, i]]):which(.data$question_name == lookup_table_cloth_block[[5, i]]),
            "question_sequence"] <- NA
      
      .data[(which(.data$question_name == lookup_table_cloth_block[[5, i]]) + 1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_cloth_block[[5, i]]) + 1):nrow(.data),
              "question_sequence"] - 5
    } # close bracket for else if{}
  } # close bracket for for(){}
  .data
} # close bracket for function(){}