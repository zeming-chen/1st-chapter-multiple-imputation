library(tidyverse)


replace_d25_answer <- function(.data) {
  
  last_q <- .data %>% pull(last_question_filled) %>% unique()
  
  # if the respondent broke off before seeing the question d25
  # do nothing and return the original data
  if (last_q %in% c("q1", "q2", "d19", "d22")) {
    .data
  } else {
    # if the respondent saw the question d25
    # extract and store this person's id 
    case_id <- .data %>% pull(case_id) %>% unique()
    
    # get the correct answer to d25 from data_to_recode for this person
    d25_na <- 
      data_recode %>% 
      filter(nid == case_id) %>% 
      pull(d25_na)
    
    # replace the originally wrong answer of d25 with the correct one
    .data$answer[.data$question_name == "d25"] <- d25_na
    
    .data
  }
}

