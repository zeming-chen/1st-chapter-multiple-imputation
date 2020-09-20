#   Table of Content
#   
#   1. prepare the work space
#
#   2. create a dataset to record question characteristics
#
#   3. code question names
#     3.1 get question names - block d and e
#     3.2 get question names - block a, b and c
#     3.3 get question names - block f
#     3.4 add question name to the data of question characteristics
#
#   4. code open-ended questions
#
#   5. code transition/introduction question
#
#   6. code question topic
#
#   7. code matrix question
#
#   8. code word counts of question stems
#     8.1 count words for questions in the questionnaire pdf
#     8.2 count words for questions missing from the questionnaire pdf
#     8.3 add word count to the data of question characteristics 
#
#   9. correct variable types
#
#   10. export data




# ================== 1. prepare the work space ==================
getwd()

library(tidyverse)

load("./data/derived/data-question-sequence-out-of-order.RData")



# ================== 2. create a dataset to record question characteristics ==================
data_meta <- tibble()




# ================== 3. code question names ==================

# ================== 3.1 get question names - block d and e ==================
vector_question_name_de_block <- 
  data_interim %>%
  select(q1:ceintro,
         -matches("hh[1-4]?$"),
         -d25o,
         -white,
         -matches("d25c[0-8]{1,2}$"),
         -matches("huhas[2-7]")) %>% 
  rename(q2 = "hhsize",
         d19 = "age.x",
         d22 = "hispanic",
         d25 = "d25c9",
         d28 = "d25c9o",
         e10 = "huhas1") %>% 
  colnames()


# ================== 3.2 get question names - block a, b and c ==================
vector_question_name_abc_block <-
  pmap_dfc(
    # specify the order of columns
    list(
      # function input 1 (used as ..1 below)
      paste0("filter", 1:16),
      # function input 2 (used as ..2 below)
      c(paste0("a", 1:6, "_1desc"),
        paste0("b", 1:5, "_2"),
        paste0("c", 1:5, "_2")),
      # function input 3 (used as ..3 below)
      c(paste0("a", 1:6, "_15"),
        paste0("b", 1:5, "_5o"),
        paste0("c", 1:4, "_15"),
        "price16")
    ), # close bracket for list()
    # function: select the columns according to the order specified above
    function(..1, ..2, ..3) select(data_interim, ..1, ..2:..3)
    ) %>% # close bracket for pmap_dfc()
    # remove extraneous questions
  select(-matches("c[1-5]_[0-9]+o$"),
         -matches("c[1-5]_[0-9]+desco$")) %>% 
  colnames()

# add three missing questions: "a1", "b1" and "c1" to the beginning of the question block
vector_question_name_abc_block <- 
  c("a1", 
    # block a
    vector_question_name_abc_block %>% 
      .[str_which(., "filter1$"):str_which(., "a6_15")],
    "b1",
    # block b
    vector_question_name_abc_block %>% 
      .[str_which(., "filter7"):str_which(., "b5_5o")],
    "c1",
    # block c
    vector_question_name_abc_block %>% 
      .[str_which(., "filter12"):str_which(., "price16")]
  ) # close bracket for c()
  

# ================== 3.3 get question names - block f ==================
vector_question_name_f_block <- 
  data_interim %>%
  select(f2:f49) %>% 
  rename(f10 = "hhinccat") %>% 
  colnames()


# ================== 3.4 add question name to the data of question characteristics ==================
data_meta <- 
  tibble(question_name = c(vector_question_name_de_block,
                           vector_question_name_abc_block,
                           vector_question_name_f_block))




# ================== 4. code open-ended questions ==================
# get names of open-ended questions
vector_open_questions <-
  c("q2",
    "d19", "d28",
    "e2o", "e5", "e6", "e7", "e21_1", "e21_2",
    data_meta %>% pull(question_name) %>% str_subset("^a.+desc$"),
    data_meta %>% pull(question_name) %>% str_subset("^a._14"),
    data_meta %>% pull(question_name) %>% str_subset("^(b|c)._2$"),
    data_meta %>% pull(question_name) %>% str_subset("^b._5o"),
    data_meta %>% pull(question_name) %>% str_subset("^c.+desc$"),
    data_meta %>% pull(question_name) %>% str_subset("^c.+15$"),
    data_meta$question_name %>% str_subset("price"))

# code open-ended questions
data_meta <- 
  data_meta %>% 
  mutate(open_ended = case_when(question_name %in% vector_open_questions ~ 1,
                                TRUE ~ 0))
  



# ================== 5. code transition/introduction question ==================
data_meta <- 
  data_meta %>% 
  mutate(intro_page = case_when(question_name %in% c("q1", "ceintro", "a1", "b1", "c1", "f2") ~ 1,
                                TRUE ~ 0))




# ================== 6. code question topic ==================
data_meta <- 
  data_meta %>% 
  mutate(topic = case_when(question_name %in% c("q1", "ceintro") ~ "commitment page",
                           question_name %in% str_subset(.$question_name, "^d|q2") ~ "demographics",
                           question_name %in% str_subset(.$question_name, "^e") ~ "housing",
                           row_number()  %in% str_which(.$question_name, "a1$"):str_which(.$question_name, "a6_15") ~ "clothing",
                           row_number()  %in% str_which(.$question_name, "b1$"):str_which(.$question_name, "b5_5o") ~ "utilities",
                           row_number()  %in% str_which(.$question_name, "c1$"):str_which(.$question_name, "price16") ~ "insurance",
                           TRUE ~ "income"))




# ================== 7. code matrix question ==================
data_meta <- 
  data_meta %>% 
  mutate(matrix = case_when(question_name %in% c("d25", "e10") ~ 1,
                            TRUE ~ 0))




# ================== 8. code word counts of question stems ==================
# note:
# word counts of questions in block d, e and f can be calculated directly from the questionnaire pdf
# however, for questions in block a, b and c, the questionnaire pdf does not include the full question texts
# thus, the calculation of the word count will consist of 2 parts:
# part 1: count words for questions in the questionnaire pdf
# part 2: count words for questions missing from the questionnaire pdf

# ================== 8.1 count words for questions in the questionnaire pdf ==================
# extract question wording from the questionnaire pdf 
vector_text_of_question_stem <-
  data_questionnaire %>%
  # collapse the texts imported from the pdf into a character of length one
  str_c(collapse = "") %>% 
  # remove un-needed texts at the start of the questionnaire (i.e., survey response instruction)
  str_remove(regex(".+(?=\nQ1)", dotall = TRUE))  %>% 
  # remove un-needed texts at the end of the questionnaire (i.e., thank you message)
  str_remove(regex("(?<=Prefer not to say)\nEND.+", dotall = TRUE)) %>% 
  # remove the explanations of the question branching route in the questionnaire
  # the explanations have this pattern: it starts with an open bracket followed by the text "if" or "IF" and ends with a closing bracket
  str_remove_all("\\([Ii][Ff].+\\)") %>% 
  # remove the comments in the questionnaire
  # the comments have this pattern: it is enclosed by a pair of square brackets
  str_remove_all("\\[.+\\]") %>% 
  # remove explanations of the requirement of the numeric input to the open-ended questions
  # the explanations have this pattern: it starts with one of the three words: "Enter", "Constrain" or "6-digit"
  str_remove_all("(?<=\n)(Constrain|Enter|6-digit).+(?=\n)") %>% 
  # remove the texts "DISPLAY ...."
  str_remove_all("DISPLAY.+(?=\n)") %>% 
  # remove explanation of the go-again-loop when giving "Yes" to the filter questions
  str_remove_all("\nI[Ff].+") %>% 
  # remove answer options - step 1
  str_remove_all("\n\\d.+") %>% 
  # remove answer options - step 2 (question E10 is a matrix and has some nested answer options that can't be detected in step 1)
  str_remove_all("\n\\s+\\d.+") %>% 
  # remove answer option - step 3 
  # after each introduction to a new question block topic, there is always a commitment statement
  # following this statement is a radio button that respondents can click to make the commitment required
  # this radio button is written in text in the questionnaire and needs to be discarded  
  str_remove_all(regex("\n(RADIO|Next).+", ignore_case = TRUE)) %>% 
  # remove the explanation of the limitation of textual inputs to the open-ended questions
  str_remove_all("(\nOPEN.+)?\n[Ll]imit input to.+") %>% 
  # remove the texts "The list of months is reduced to ...."
  str_remove_all("\nThe list of months is reduced to.+\n.+\\)") %>% 
  # remove the question block names
  str_remove_all("\n(Housing|Clothing|Utilities|Non-Health|Income).+") %>% 
  # change the texts "(reference ... months)" to just one word: "MONTH"
  str_replace_all("\\(reference of the (current|past)(\\s)?(\n?three )?month\\)", "MONTH") %>% 
  # change the texts "(reference ... last day... month)" to just one word: "DAY"  
  str_replace_all("\\(reference of\\s?\n?the\\s?\n?last day of the past month\\)", "DAY") %>% 
  # change the texts "(...year)" to just one word: "YEAR"  
  str_replace_all("\\((current|past) year\\)", "YEAR") %>%
  # replace the square brackets enclosing the texts "Do you/ Does any member of your household" to round brackets
  str_replace_all("<Do you/ Does any member of your household>",
                  "(Do you/ Does any member of your household)") %>%  
  # change the texts "(FILL C1.4)" to "(FILL)"  
  str_replace_all("(?<=\\(FILL)\\sC1.4(?=\\))", "") %>% 
  # remove "\n"
  str_replace_all("\n", " ")

# count words in the question stems
data_question_stem_word_count <-
  tibble(
    # extract the question names that are used in the questionnaire pdf
    question_name = 
      vector_text_of_question_stem %>% 
      str_extract_all("CEINTRO|[A-Z][0-9]+(\\.[A-Z0-9]+)?(_other)?") %>% 
      unlist() %>% 
      str_to_lower(),
    
    # count the number of words in for each question stem
    word_count = 
      vector_text_of_question_stem %>% 
      str_split("CEINTRO|[A-Z][0-9]+(\\.[A-Z0-9]+)?(_other)?", simplify = TRUE) %>% 
      map_dbl( ~ str_count(.x, "\\w+")) %>% 
      # remove the first element as it is an extraneous product from the coding
      .[-1]
  ) # close bracket for tibble()

# correct some mistakes in the word counts
data_question_stem_word_count <- 
  data_question_stem_word_count %>% 
  mutate(word_count = case_when(question_name == "d29"  ~ 14,
                                question_name == "e3"   ~ 1,
                                question_name == "e10"  ~ 30,
                                TRUE ~ as.numeric(word_count)))




# ================== 8.2 count words for questions missing from the questionnaire pdf ==================

# to will calculate the word counts for those missing filter and follow-up question pairs in block a, b and c,
# the following codes follows this logic:
# for each question block a, b and c
# we know the word count of the first filter and its follow-up questions (because they are fully written in the questionnaire pdf)
# luckily, the missing filter and follow-up questions in the same question block only differ by the subjects the questions are asking for
# (e.g., "jewellery" changes to "coat", "pants" etc.)
# in other words, the missing filter and follow-up questions only differ from the first filter and follow-up question pair by a few words
# thus, the following codes record the number to subtract from (or add to) the word count of first filter and follow-up question pair
# and then loop over each subtraction/addition to get the word count for each missing filter and follow-up question pair

list_word_count_abc_blocks <-
  list(
    .start = list("a1.1$", "b1.1$", "c1.1$"),
    .end = list("a1.15", "b1.5_other", "c1.15"),
    .number_to_subtract = list(
      # substraction/addition for block a
      # each column represents a par of filter and follow-up questions
      tibble(
        V1 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        V2 = c(-5, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
        V3 = c(-6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        V4 = c(-9,-3, 0, 0, 0, 0,-3,-3, 0, 0, 0, 0),
        V5 = c(-6,-3, 0, 0, 0, 0,-3,-3, 0, 0, 0, 0),
        V6 = c(-9,-3, 0, 0, 0, 0,-3,-3, 0, 0, 0, 0)),
      # substraction/addition for block b
      tibble(
        V1 = c(0, 0, 0, 0, 0, 0),
        V2 = c(3, 3, 3, 3, 0, 0),
        V3 = c(1, 1, 1, 1, 0, 0),
        V4 = c(5, 5, 5, 5, 0, 0),
        V5 = c(3, 3, 3, 3, 0, 0)),
      # substraction/addition for block c
      tibble(
        V1 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        V2 = c(-11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        V3 = c(-11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        V4 = c(-8, 3, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0),
        V5 = c(-7, 4, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA))
    ) # close bracket for .number_to_subtract = list()
  ) %>% # close bracket for list()
  pmap(function(.start, .end, .number_to_subtract) {
    data_question_stem_word_count %>%
      filter(
        row_number() %in% str_which(.$question_name, .start):str_which(.$question_name, .end)
      ) %>%
      bind_cols(.number_to_subtract) %>%
      pivot_longer(cols = starts_with("V"),
                   names_to = "column_name_original",
                   values_to = "difference") %>%
      arrange(column_name_original) %>%
      mutate(word_count_updated = word_count + difference) %>%
      filter(!is.na(word_count_updated)) %>% 
      select(question_name, word_count = word_count_updated)
  }) %>% 
  # set names for the list element for later reference
  set_names(nm = c("block_a", "block_b", "block_c"))


# ================== 8.3 add word count to the data of question characteristics ==================
# combine all word counts into one vector
vector_word_count_question_stem <-
  bind_rows(
    # block d and e
    data_question_stem_word_count %>%
      filter(row_number() %in% str_which(.$question_name, "q1"):str_which(.$question_name, "a1$")),
    # block a
    list_word_count_abc_blocks$block_a,
    # question b1
    data_question_stem_word_count %>% filter(question_name == "b1"),
    # block b
    list_word_count_abc_blocks$block_b,
    # question c1
    data_question_stem_word_count %>% filter(question_name == "c1"),
    # block c
    list_word_count_abc_blocks$block_c,
    # block f
    data_question_stem_word_count %>%
      filter(row_number() %in% str_which(.$question_name, "f2$"):str_which(.$question_name, "f49"))
  ) %>% # close bracket for bind_rows()
  pull()

# add a column of question stem word count to the data of question characteristics
data_meta <- 
  data_meta %>% 
  mutate(word_count = vector_word_count_question_stem)




# ================== 9. correct variable types ==================
data_meta <-   
  data_meta %>% 
  mutate(intro_page = factor(intro_page, 
                             levels = c(0, 1), 
                             labels = c("No", "Yes")),
         topic = factor(topic, 
                        levels = c("commitment page", "demographics", "housing", "clothing", "utilities", "insurance", "income"),
                        labels = c("commitment page", "demographics", "housing", "clothing", "utilities", "insurance", "income")),
         matrix = factor(matrix, 
                         levels = c(0, 1), 
                         labels = c("No", "Yes")),
         open_ended = factor(open_ended, 
                             levels = c(0, 1), 
                             labels = c("No", "Yes")))

# check the coding result
data_meta %>% summary()




# ================== 10. export data ==================
save(data_meta, file = "./data/derived/data-meta.RData")


