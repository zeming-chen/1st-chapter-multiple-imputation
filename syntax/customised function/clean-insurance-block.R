library(tidyverse)


# create a table for the function below to loop over for cleaning filter question
lookup_table_insurance_block <-
  rbind(paste0("c", 1:5, "_2"),
        paste0("price", 12:16),
        paste0("c", 1:5, "_3"),
        paste0("c", 1:5, "_4"),
        paste0("price", 12:16)) %>% 
  as_tibble() %>%
  set_names(paste0("filter", 12:16))

# create another table for the function below to loop over for cleaning go-gain loop questions
lookup_table_insurance_faddl_questions <-        
  rbind(paste0("faddl", 12:15),
        paste0("c", 1:4, "_12"),
        paste0("c", 1:4, "_15"),
        paste0("c", 1:4, "_13"),
        paste0("c", 1:4, "_14"),
        paste0("c", 1:4, "_15")) %>% 
  as_tibble() %>%
  set_names(paste0("filter", 12:15))


clean_insurance_block <- function(.data) {
  
  for (i in paste0("filter", 12:15)) {
    
    if (!.data[.data$question_name == i, "answer"] %in% "Yes") {
      
      .data[which(.data$question_name == lookup_table_insurance_faddl_questions[[1, i]]):which(.data$question_name == lookup_table_insurance_faddl_questions[[3, i]]),
            "question_sequence"] <- NA
      
      .data[(which(
        .data$question_name == lookup_table_insurance_faddl_questions[[3, i]]
      ) +
        1):nrow(.data),
      "question_sequence"] <-
        .data[(which(
          .data$question_name == lookup_table_insurance_faddl_questions[[3, i]]
        ) +
          1):nrow(.data),
        "question_sequence"] - 6
    }
    
    if (.data[.data$question_name == i, "answer"] %in% "Yes" &
        !.data[.data$question_name == lookup_table_insurance_faddl_questions[[1, i]], "answer"] %in% "Yes") {
      .data[which(.data$question_name == lookup_table_insurance_faddl_questions[[2, i]]):which(.data$question_name == lookup_table_insurance_faddl_questions[[3, i]]),
            "question_sequence"] <- NA
      
      .data[(which(
        .data$question_name == lookup_table_insurance_faddl_questions[[3, i]]
      ) +
        1):nrow(.data),
      "question_sequence"] <-
        .data[(which(
          .data$question_name == lookup_table_insurance_faddl_questions[[3, i]]
        ) +
          1):nrow(.data),
        "question_sequence"] - 5
      
    }
    
    if (.data[.data$question_name == i, "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_faddl_questions[[1, i]], "answer"] %in% "Yes" &
        !.data[.data$question_name == lookup_table_insurance_faddl_questions[[4, i]], "answer"] %in% "Yes") {
      
      .data[.data$question_name == lookup_table_insurance_faddl_questions[[5, i]], "question_sequence"] <- NA
      .data[.data$question_name == lookup_table_insurance_faddl_questions[[6, i]], "question_sequence"] <- NA
      
      .data[(which(
        .data$question_name == lookup_table_insurance_faddl_questions[[6, i]]
      ) +
        1):nrow(.data),
      "question_sequence"] <-
        .data[(which(
          .data$question_name == lookup_table_insurance_faddl_questions[[6, i]]
        ) +
          1):nrow(.data),
        "question_sequence"] - 2
      
    }
    
    if (.data[.data$question_name == i, "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_faddl_questions[[1, i]], "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_faddl_questions[[4, i]], "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_faddl_questions[[5, i]], "answer"] %in% c(NA, "Other", "other", ".d", ".r")) {
      
      .data[.data$question_name == lookup_table_insurance_faddl_questions[[6, i]], "question_sequence"] <- NA
      
      .data[(which(
        .data$question_name == lookup_table_insurance_faddl_questions[[6, i]]
      ) +
        1):nrow(.data),
      "question_sequence"] <-
        .data[(which(
          .data$question_name == lookup_table_insurance_faddl_questions[[6, i]]
        ) +
          1):nrow(.data),
        "question_sequence"] - 1
      
    }
    
  }
  
  for (i in paste0("filter", 12:16)) {
    
    if (!.data[.data$question_name == i, "answer"] %in% "Yes") {
      .data[which(.data$question_name == lookup_table_insurance_block[[1, i]]):which(.data$question_name == lookup_table_insurance_block[[2, i]]),
            "question_sequence"] <- NA
      
      .data[(which(.data$question_name == lookup_table_insurance_block[[2, i]]) +
               1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_insurance_block[[2, i]]) +
                 1):nrow(.data),
              "question_sequence"] - 5
    }
    
    if (.data[.data$question_name == i, "answer"] %in% "Yes" &
        !.data[.data$question_name == lookup_table_insurance_block[[3, i]], "answer"] %in% "Yes") {
     
       .data[.data$question_name == lookup_table_insurance_block[[4, i]], "question_sequence"] <- NA
       .data[.data$question_name == lookup_table_insurance_block[[5, i]], "question_sequence"] <- NA
      
       .data[(which(.data$question_name == lookup_table_insurance_block[[5, i]]) +
               1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_insurance_block[[5, i]]) +
                 1):nrow(.data),
              "question_sequence"] - 2
    }
    
    
    if (.data[.data$question_name == i, "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_block[[3, i]], "answer"] %in% "Yes" &
        .data[.data$question_name == lookup_table_insurance_block[[4, i]], "answer"] %in% c(NA, "Other", "other", ".d", ".r")) {
      
        .data[.data$question_name == lookup_table_insurance_block[[5, i]], "question_sequence"] <- NA
      
        .data[(which(.data$question_name == lookup_table_insurance_block[[5, i]]) +
               1):nrow(.data),
            "question_sequence"] <-
        .data[(which(.data$question_name == lookup_table_insurance_block[[5, i]]) +
                 1):nrow(.data),
              "question_sequence"] - 1
    } # close bracket for the last if(){}
  } # close bracket for for(){}
  .data
} # close bracket for function(){}