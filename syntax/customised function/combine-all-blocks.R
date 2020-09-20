library(tidyverse)

combine_all_blocks <-
  function(de_block, abc_block, f_block) {
    # combine de and abc question blocks
    temporary_df_1 <- bind_rows(de_block, abc_block)
    
    # increase the question sequence for the abc question block by the number of questions in the de question blocks as the de blocks come first
    temporary_df_1[c(22:177), "question_sequence"] <-
      temporary_df_1[c(22:177), "question_sequence"] + as.numeric(temporary_df_1[21, "question_sequence"])
    
    # get the last question sequence in abc question block
    temporary_last_question_sequence <-
      temporary_df_1 %>%
      filter(!is.na(question_sequence)) %>%
      pull(question_sequence) %>%
      tail(1)
    
    # combine f question block and the deabc question block
    temporary_df_2 <- bind_rows(temporary_df_1, f_block)
    
    # increase the question sequence for the f question block by the number of questions in the deabc question blocks as the latter is in front of the f block
    temporary_df_2[c(178:193), "question_sequence"] <-
      temporary_df_2[c(178:193), "question_sequence"] + temporary_last_question_sequence
    
    temporary_df_2
  }
