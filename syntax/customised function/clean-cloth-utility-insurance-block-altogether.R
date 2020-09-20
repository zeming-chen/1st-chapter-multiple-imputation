library(tidyverse)


# note:
# this syntax file replies on three other syntax files, they are:
# clean-cloth-block.R
# clean-utility-block.R
# clean-insurance-block.R


# put functions to clean the three randomised question blocks into one list for the below loop
list_of_functions <-
  list(cloth      = clean_cloth_block,
       utility    = clean_utility_block,
       insurance  = clean_insurance_block)


clean_abc_block <- function(.data) {
  
  # only change the number of questions answered for those who reached the three randomised question blocks
  if (.data$block_order[1] != "breakoff before reaching randomised question blocks") {
   
    # extract the block order the respondent was exposed to 
    vector_block_order <- 
      .data %>%
      pull(block_order) %>%
      str_split(", ") %>%
      .[[1]]
    
    # automatically choose the data cleaning function corresponding to the topic of 1st block order 
    step_1 <- list_of_functions[[vector_block_order[1]]]
    # automatically choose the data cleaning function corresponding to the topic of 2nd block order
    step_2 <- list_of_functions[[vector_block_order[2]]]
    # automatically choose the data cleaning function corresponding to the topic of 3rd block order
    step_3 <- list_of_functions[[vector_block_order[3]]]
    
    # work out the correct question sequence according to the block order the respondent was exposed to 
    .data %>%
      step_1 %>%
      step_2 %>%
      step_3 %>%
      # remove the last row as it is a side effect of the loop used in cleaning the three randomised question blocks
      filter(!is.na(question_name))
    
  } else {
    # for those respondents who broke off before reaching the three randomised question blocks, 
    # their question sequences in the abc blocks remain unchanged (i.e. equal to NA which was initially assigned to)
    .data
  } # close bracket for else{}
} # close bracket for the function(){}