# Table of Content
#
# 1. prepare work space
#
# 2. code the order of question blocks
#
# 3. code respondents' last answered question
#
# 4. clean abc question blocks - grouped version
#   4.1 prepare for cleaning abc question blocks
#   4.2 get the correct question sequence for each order of question blocks
#   4.3 get respondents' answers to survey questions
#   4.4 re-order data
#   4.5 remove unanswered questions
#
# 5. clean abc question blocks - interleafed version
#   5.1 prepare for cleaning abc question blocks
#   5.2 get the correct question sequence for each order of question blocks
#   5.3 get respondents' answers to survey questions
#   5.4 re-order data
#   5.5 remove unanswered questions
#
# 6. clean de question blocks
#   6.1 prepare for cleaning de question blocks
#   6.2 get the correct question sequence in de question blocks
#   6.3 get respondents' answers to survey questions
#   6.4 remove unanswered questions
#
# 7. clean f question block
#   7.1 prepare for cleaning f question block
#   7.2 get the correct question sequence f de question block
#   7.3 get respondents' answers to survey questions
#   7.4 remove unanswered questions
#
# 8. join the all question blocks
#
# 9. add a1b1c1 as new rows
#
# 10. remove questions that the respondents did not answer
#
# 11. export data




# ======== 1. prepare work space ========
library(tidyverse)

getwd()

# load the data to be cleaned
load("./data/derived/data-question-sequence-out-of-order.RData")

# load customised functions to clean the data
list.files("./syntax/customised function") %>% 
  str_subset("clean-cloth-utility-insurance-block-altogether.R", negate = T) %>% 
  walk( ~ source(str_c("./syntax/customised function/", .x)))

source("./syntax/customised function/clean-cloth-utility-insurance-block-altogether.R")




# ======== 2. code the order of question blocks ========
# check all combinations of the question block orders
data_interim %>%
  select(ends_with("order")) %>%
  group_by_all() %>%
  count() %>%
  print(n = nrow(.))
# find:
# there are some missing data in the columns of block order
# mainly because respondents broke off before reaching that question block
# thus, block order needs to be re-coded to account for data missingness due to this reason

# code the block order to which respondents were assigned
data_interim <-
  data_interim %>%
  mutate(block_order = case_when(
    # scenario 1: cloth, utility, insurance
    clothing_order == 1 & utility_order == 2 & insurance_order == 3 ~ "cloth, utility, insurance",
    clothing_order == 1 & utility_order == 2 & insurance_order %in% NA ~ "cloth, utility, insurance",
    clothing_order == 1 & utility_order %in% NA & insurance_order %in% NA ~ "cloth, utility, insurance",
    clothing_order == 9 & utility_order == 9 & insurance_order == 9 ~ "cloth, utility, insurance",
    clothing_order == 9 & utility_order %in% NA & insurance_order %in% NA ~ "cloth, utility, insurance",
    # scenario 2: cloth, insurance, utility
    clothing_order == 1 & utility_order == 3 & insurance_order == 2 ~ "cloth, insurance, utility",
    clothing_order == 1 & utility_order %in% NA & insurance_order == 2 ~ "cloth, insurance, utility",
    # scenario 3: utility, cloth, insurance
    clothing_order == 2 & utility_order == 1 & insurance_order == 3 ~ "utility, cloth, insurance",
    clothing_order == 2 & utility_order == 1 & insurance_order %in% NA ~ "utility, cloth, insurance",
    # scenario 4: utility, insurance, cloth
    clothing_order == 3 & utility_order == 1 & insurance_order == 2 ~ "utility, insurance, cloth",
    clothing_order %in% NA & utility_order == 1 & insurance_order == 2 ~ "utility, insurance, cloth",
    clothing_order %in% NA & utility_order == 9 & insurance_order == 9 ~ "utility, insurance, cloth",
    # scenario 5: insurance, cloth, utility
    clothing_order == 2 & utility_order == 3 & insurance_order == 1 ~ "insurance, cloth, utility",
    clothing_order == 2 & utility_order %in% NA & insurance_order == 1 ~ "insurance, cloth, utility",
    # scenario 6: insurance, utility, cloth
    clothing_order == 3 & utility_order == 2 & insurance_order == 1 ~ "insurance, utility, cloth",
    clothing_order %in% NA & utility_order == 2 & insurance_order == 1 ~ "insurance, utility, cloth",
    # treat the following two situations as scenario 4
    clothing_order %in% NA & utility_order == 1 & insurance_order %in% NA ~ "utility, insurance, cloth",
    clothing_order %in% NA & utility_order == 9 & insurance_order %in% NA ~ "utility, insurance, cloth",
    # treat the following two situations as scenario 5
    clothing_order %in% NA & utility_order %in% NA & insurance_order == 1 ~ "insurance, cloth, utility",
    clothing_order %in% NA & utility_order %in% NA & insurance_order == 9 ~ "insurance, cloth, utility",
    # scenario 7: some people broke off before reaching the randomised sections
    TRUE ~ "breakoff before reaching randomised question blocks") # close bracket for case_when()
  ) # close bracket for mutate()




# ======== 3. code respondents' last answered question ========
# check the missing data in the column of last question filled

# examine how many questions were answered for those respondents who have missing data in their last question filled
data_interim %>%
  filter(is.na(lastquestionfilled)) %>%
  select(q1:f49) %>%
  map_dbl( ~ sum(is.na(.))) %>%
  unique()
# find:
# it seems that all these respondents did not answer any question
# but all of them seem to answer some questions
# need to figure out what these questions are

data_interim %>%
  filter(is.na(lastquestionfilled)) %>%
  select(q1:f49) %>%
  map_dbl( ~ sum(is.na(.))) %>% 
  keep(. < 104)
# find:
# these questions are: 
# "huhas1", "huhas2", "huhas3", "huhas4", "huhas5", "huhas6", "huhas7"
# need to check why they all answer these questions

data_interim %>%
  filter(is.na(lastquestionfilled)) %>%
  select(huhas1:huhas7) %>%
  map(~ sum(.x != 0))
# find:
# all answers in "huhas1" to "huhas7" have a value of 0
# 0 is not respondent's answer
# rather, it is the way of coding missing data in these 7 variables


# conclusion:
# all respondents with missing data in the column "lastquestionfilled" did not answer any single question
# it means all of them broke off at the 1st question they saw
# I will replace the NA in their last question with "q1"


# create a new column to record respondents' last answered question 
data_interim <-
  data_interim %>%
  mutate(last_question_filled = str_to_lower(lastquestionfilled))


# reconcile the value in the column "last_question_filled" with the value used later
data_interim <-
  data_interim %>%
  mutate(last_question_filled = map_chr(last_question_filled, 
                                        ~ recode_last_question_filled(.x)))


# ======== 4. clean abc question blocks - grouped ========

# ======== 4.1 prepare for cleaning abc question blocks ========

# select data of grouped version only
data_grouped <- 
  data_interim %>%
  filter(grouped == "Grouped")

# select the questions in abc blocks
data_grouped_abc_question_blocks <-
  data_grouped %>%
  select(nid,
         block_order,
         filter1:price16,
         -matches("clothing|utility|insurance"),
         -matches("c[1-5]_[0-9]+o$"),
         -matches("c[1-5]_[0-9]+desco$"))

# get the total number of questions in each a, b and c question block
vector_total_questions_in_each_abc_question_block <- 
  c(
  # get the total number of questions in the cloth block
  cloth = data_grouped_abc_question_blocks %>% select(filter1:a6_15) %>% ncol(),
  # get the total number of questions in the utility block
  utility = data_grouped_abc_question_blocks %>% select(filter7:b5_5o) %>% ncol(),
  # get the total number of questions in the insurance block
  insurance = data_grouped_abc_question_blocks %>% select(filter12:price16) %>% ncol()
  )


# ====== 4.2 get the correct question sequence for each order of question blocks ======
# create long data with info on the block order respondents were assigned to
data_grouped_long_abc_question_blocks <- 
  tibble(
    case_id =
      data_grouped_abc_question_blocks$nid %>% rep(each = colnames(data_grouped_abc_question_blocks) %>% .[-c(1, 2)] %>% length()),
    question_name = 
      colnames(data_grouped_abc_question_blocks) %>% .[-c(1, 2)] %>% rep(data_grouped_abc_question_blocks$nid %>% length()),
    block_order = 
      data_grouped_abc_question_blocks$block_order %>% rep(each =  colnames(data_grouped_abc_question_blocks) %>% .[-c(1, 2)] %>% length())
    )


# create a lookup table
# which contains different question sequences for different block orders
lookup_table_for_question_sequence_grouped <-
  # work out the correct question sequence for each section order
  data_grouped_abc_question_blocks %>%
  pull(block_order) %>%
  unique() %>%
  # there are some respondents who broke off before reaching the randomised question blocks
  # the value in the column "block_order" for them is "breakoff before reaching randomised question blocks"
  # it should be removed when deriving the question sequence of those reaching the randomised question blocks.
  .[. != "breakoff before reaching randomised question blocks"] %>%
  map_dfc(~ derive_question_sequence(.)) %>%
  # add a column for the respondents who did not reach the randomised question blocks 
  # so that the question sequence in the three randomised blocks for them is all NA
  mutate("breakoff before reaching randomised question blocks" = NA_integer_) %>%
  # add a column for the question name
  mutate(question_name = colnames(data_grouped_abc_question_blocks) %>%
           # the first 2 elements are not questions in the survey so they need to be excluded
           .[-c(1, 2)]) %>%
  # re-order the columns
  relocate(question_name, .before = 1) %>%
  # transform the data from wide to long format
  gather(block_order, question_sequence, -question_name)


# append the column of question sequence from the lookup table to long data created above 
# (i.e. data_grouped_long_abc_question_blocks)
data_grouped_abc_question_blocks_with_question_sequence <-
  left_join(data_grouped_long_abc_question_blocks,
            lookup_table_for_question_sequence_grouped,
            by = c("question_name", "block_order"))


# ======= 4.3 get respondents' answers to survey questions =======
# transform the survey data to long format 
# this long data has info on respondents' answers to all questions they saw
data_grouped_transposed_abc_question_blocks <-
  # transpose data
  # such that columns represent respondents and rows are question items
  data_grouped_abc_question_blocks %>%
  .[, -c(1, 2)] %>%
  t() %>%
  as_tibble() %>%
  # rename column with the respondent's id
  set_names(data_grouped_abc_question_blocks$nid) %>%
  # transform the data to long format
  # such that row is a combination of respondent and question
  gather(case_id, answer) %>%
  # create columns to use as primary key when merging data later
  mutate(case_id = as.numeric(case_id),
         question_name = data_grouped_abc_question_blocks %>% colnames() %>% .[-c(1, 2)] %>% rep(nrow(data_grouped_abc_question_blocks))) %>%
  # re-order the columns
  relocate(question_name, .after = 1)


# append the column of respondents' answers
data_grouped_abc_question_blocks_to_clean <-
  left_join(data_grouped_abc_question_blocks_with_question_sequence,
            data_grouped_transposed_abc_question_blocks,
            by = c("case_id", "question_name"))


# ======= 4.4 re-order data =======
# such that the first row corresponds to the first question respondents answered
data_grouped_abc_question_blocks_to_clean <-
  data_grouped_abc_question_blocks_to_clean %>%
  group_by(case_id) %>%
  arrange(question_sequence, .by_group = TRUE) %>%
  ungroup()


# ======= 4.5 remove unanswered questions =======
data_grouped_abc_question_blocks_cleaned <-
  data_grouped_abc_question_blocks_to_clean %>%
  split(.$case_id) %>%
  map_dfr( ~ clean_abc_block(.))





# ======= 5. clean abc question blocks - interleafed =======

# ====== 5.1 prepare for cleaning abc question blocks ======
# select data of interleafed version
data_interleafed <- 
  data_interim %>%
  filter(grouped != "Grouped")


# select the questions in abc question blocks
# then re-order the questions
# such that each filter question is immediately followed by its follow-ups
data_interleafed_abc_question_blocks <-
  pmap_dfc(
    # specify the order of questions
    list(
      paste0("filter", 1:16),
      
      c(paste0("a", 1:6, "_1desc"),
        paste0("b", 1:5, "_2"),
        paste0("c", 1:5, "_2")),
      
      c(paste0("a", 1:6, "_15"),
        paste0("b", 1:5, "_5o"),
        paste0("c", 1:4, "_15"),
        "price16")
    ), # close bracket for list()

  # select the columns according to the order specified above
  function (..1, ..2, ..3) select(data_interleafed, ..1, ..2:..3)
  ) %>% # close bracket for pmap_dfc()
  # remove some unnecessary columns
  select(-matches("c[1-5]_[0-9]+o$"),
         -matches("c[1-5]_[0-9]+desco$")) %>%
  # add two columns
  add_column(nid = data_interleafed$nid,
             block_order = data_interleafed$block_order,
             .before = 1)


# ====== 5.2 get the correct question sequence for each order of question blocks ======
# create long data with info on the block order respondents were assigned to
data_interleafed_long_abc_question_blocks <- 
  tibble(
    case_id =
      data_interleafed_abc_question_blocks$nid %>%
      rep(each =  colnames(data_interleafed_abc_question_blocks) %>% .[-c(1, 2)] %>% length()),
    question_name =
      colnames(data_interleafed_abc_question_blocks) %>% .[-c(1, 2)] %>%
      rep(data_interleafed_abc_question_blocks$nid %>% length()),
    block_order =
      data_interleafed_abc_question_blocks$block_order %>%
      rep(each =  colnames(data_interleafed_abc_question_blocks) %>% .[-c(1, 2)] %>% length())
    )


# create a lookup table
# which contains different question sequences for different block orders
lookup_table_for_question_sequence_interleafed <-
  # work out the correct question sequence for each section order
  data_interleafed_abc_question_blocks %>%
  pull(block_order) %>%
  unique() %>%
  # there are some respondents who broke off before reaching the randomised question blocks, and the value for them in the column "block_order" is "breakoff before reaching randomised question blocks". It should be removed when deriving the question sequence of those reaching the randomised question blocks.
  .[. != "breakoff before reaching randomised question blocks"] %>%
  map_dfc(~ derive_question_sequence(.)) %>%
  # add a column for the respondents who did not reach the randomised question blocks so that the question sequence in the three randomised blocks for them is all NA
  mutate("breakoff before reaching randomised question blocks" = NA_integer_) %>%
  # add a column for the question name
  mutate(question_name = colnames(data_interleafed_abc_question_blocks) %>%
           # the first 2 elements are not questions in the survey so they need to be excluded
           .[-c(1, 2)]) %>%
  # re-order the columns
  select(question_name, everything()) %>%
  # transform the data from wide to long format
  gather(block_order, question_sequence, -question_name)


# append the column of question sequence from the lookup table to long data created above 
# (i.e. data_interleafed_long_abc_question_blocks)
data_interleafed_abc_question_blocks_with_question_sequence <-
  left_join(data_interleafed_long_abc_question_blocks,
            lookup_table_for_question_sequence_interleafed,
            by = c("question_name", "block_order"))


# ====== 5.3 get respondents' answers to survey questions ======
# transform the survey data to long format 
# this long data has info on respondents' answers to all questions they saw
data_interleafed_transposed_abc_question_blocks <-
  # create a dataset where columns represent respondents and rows are question indexes
  data_interleafed_abc_question_blocks %>%
  .[, -c(1, 2)] %>%
  t() %>%
  as_tibble() %>%
  # rename column with the id numbers
  set_names(data_interleafed_abc_question_blocks$nid) %>%
  # transform the data to long format (i.e. rows are the combinaiton between respondent and question name)
  gather(case_id, answer) %>%
  # create columns that are in common with the data for data linkage later
  mutate(case_id = as.numeric(case_id),
         question_name = data_interleafed_abc_question_blocks %>% colnames() %>% .[-c(1, 2)] %>% rep(nrow(data_interleafed_abc_question_blocks))) %>%
  # re-order the columns
  select(case_id, question_name, everything())


# append the column of respondents' answers
data_interleafed_abc_question_blocks_to_clean <-
  left_join(data_interleafed_abc_question_blocks_with_question_sequence,
            data_interleafed_transposed_abc_question_blocks,
            by = c("case_id", "question_name"))


# ====== 5.4 re-order data ======
# such that the first row corresponds to the first question respondents answered
data_interleafed_abc_question_blocks_to_clean <-
  data_interleafed_abc_question_blocks_to_clean %>%
  group_by(case_id) %>%
  arrange(question_sequence, .by_group = TRUE) %>%
  ungroup()


# ==== 5.5 remove unanswered questions ====
data_interleafed_abc_question_blocks_cleaned <-
  data_interleafed_abc_question_blocks_to_clean %>%
  split(.$case_id) %>%
  map_dfr( ~ clean_abc_block(.))




# ====== 6. clean de question blocks ====

# ====== 6.1 prepare for cleaning de question blocks ====

# check whether breakoff is recorded at the matrix question level
# or at the level of its detailed item
# (e.g., D25 vs. D25_1, D25_2, D25_3...)
data_interim %>%
  pull(lastquestionfilled) %>%
  unique() %>%
  str_subset("^(A|B|C|ENDING)", negate = TRUE)
# find:
# breakoff position is recorded at the matrix level
# thus, only need to use one of the detailed item to represent the entire matrix


# check each detailed item should be used to represent its matrix question

# household size
data_interim %>%
  select(hhsize, hh, paste0("hh", 2:4)) %>%
  group_by_all() %>%
  count()
# find:
# no one has answered the question "hh", "hh2", "hh3" or "hh4"
# these four questions should be ignored
# use "hhsize" to represent the entire matrix on household size

# ethnicity
data_interim %>%
  select(white, matches("d25c[0-9]{1,2}$"), d25c9o) %>%
  group_by_all() %>%
  count() %>%
  print(n = nrow(.))
# find: 
# use "d25c9/d25" to represent the entire matrix on respondents' ethnicity

# household has ... facilities
data_interim %>%
  select(matches("huhas[1-7]$")) %>%
  group_by_all() %>%
  count() %>%
  print(n = nrow(.))
# find:
# should create a new variable called "e10" to represent the entire matrix on household facility


# select the questions in de question blocks
data_before_randomised_blocks <- 
  data_interim %>%
  select(nid,
         grouped,
         q1:ceintro,
         -matches("hh[1-4]?$"),
         # remove d25o because it is not a question in the questionnaire 
         # but a derived variable after the survey is done
         -d25o,
         -white, -matches("d25c[0-8]{1,2}$"),
         -matches("huhas[2-7]")) %>%
  # rename some columns 
  # such that they have the same name as the breakoff position recorded in the column "lastquestionfilled"
  rename(q2 = "hhsize",
         d19 = "age.x",
         d22 = "hispanic",
         d25 = "d25c9",
         d28 = "d25c9o",
         e10 = "huhas1")


# ====== 6.2 get the correct question sequence in de question blocks ======
data_long_de_question_blocks <- 
  tibble(
    case_id =
      data_before_randomised_blocks$nid %>% rep(each =  colnames(data_before_randomised_blocks) %>% .[-c(1,2)] %>% length()),
    question_name = 
      colnames(data_before_randomised_blocks) %>% .[-c(1,2)] %>% rep(data_before_randomised_blocks$nid %>% length()),
    question_sequence = 
      c(1:21) %>% rep(data_before_randomised_blocks$nid %>% length()),
    filter_format = 
      data_before_randomised_blocks$grouped %>% rep(each = colnames(data_before_randomised_blocks) %>% .[-c(1,2)] %>% length())
    ) 


# ====== 6.3 get respondents' answers to survey questions ======
data_transposed_de_question_blocks <-
  data_before_randomised_blocks %>%
  .[, -c(1,2)] %>%
  t() %>%
  as_tibble() %>%
  # rename column names as the case id numbers
  set_names(data_before_randomised_blocks$nid) %>%
  # transform the data to lthe ong format (i.e. rows are the combinaiton between respondebts and question names)
  gather(case_id, answer) %>%
  # create columns that are in common with the data to link to in the future
  mutate(case_id = as.numeric(case_id),
         question_name = data_before_randomised_blocks %>%  colnames() %>% .[-c(1,2)] %>% rep(nrow(data_before_randomised_blocks))) %>%
  # re-order the columns
  select(case_id, question_name, everything())


# append the column of respondents' answers
data_de_question_blocks_to_clean <-
  left_join(data_long_de_question_blocks,
            data_transposed_de_question_blocks,
            by = c("case_id", "question_name"))


# ==== 6.4 remove unanswered questions ====
data_de_question_blocks_cleaned <-
  data_de_question_blocks_to_clean %>%
  split(.$case_id) %>%
  map_dfr(~ clean_de_blocks(.))




# ====== 7. clean f question block ====

# ====== 7.1 prepare for cleaning f question block ====

# check the column name in f question block
data_interim %>%
  select(nid, f2:f49) %>%
  glimpse()
# find:
# name of every column is fine except for "hhinccat"
# "hhinccat" should be renamed as "f10"

# select the questions in f question block
data_after_randomised_blocks <-
  data_interim %>%
  select(nid, grouped, f2:f49) %>%
  rename(f10 = "hhinccat")


# ====== 7.2 get the correct question sequence in f question block ======
data_long_f_question_blocks <- 
  tibble(
    case_id =
      data_after_randomised_blocks$nid %>% rep(each =  colnames(data_after_randomised_blocks) %>% .[-c(1,2)] %>% length()),
    question_name =
      colnames(data_after_randomised_blocks) %>% .[-c(1,2)] %>% rep(data_after_randomised_blocks$nid %>% length()),
    question_sequence = 
      c(1:16) %>% rep(data_after_randomised_blocks$nid %>% length()),
    filter_format = 
      data_after_randomised_blocks$grouped %>% rep(each = colnames(data_after_randomised_blocks) %>% .[-c(1,2)] %>% length())
  )


# ====== 7.3 get respondents' answers to survey questions ======
data_transposed_f_question_blocks <-
  data_after_randomised_blocks %>%
  .[, -c(1,2)] %>%
  t() %>%
  as_tibble() %>%
  # rename column names as the case id numbers
  set_names(data_after_randomised_blocks$nid) %>%
  # transform the data to long format (i.e. rows are the combinaiton between case id and question index)
  gather(case_id, answer) %>%
  # create columns that are in common with the data to link to in the future
  mutate(case_id = as.numeric(case_id),
         question_name = data_after_randomised_blocks %>%  colnames() %>% .[-c(1,2)] %>% rep(nrow(data_after_randomised_blocks))) %>%
  # re-order the columns
  select(case_id, question_name, everything())


# append the column of respondents' answers
data_f_question_blocks_to_clean <-
  left_join(data_long_f_question_blocks,
            data_transposed_f_question_blocks,
            by = c("case_id", "question_name"))


# ====== 7.4 remove unanswered questions ======
data_f_question_blocks_cleaned <-
  data_f_question_blocks_to_clean %>%
  split(.$case_id) %>%
  map_dfr( ~ clean_f_blocks(.))



# ====== 8. join the all question blocks ======
# split data by id to use as arguments in the function below
data_de_blocks <-
  data_de_question_blocks_cleaned %>%
  split(.$case_id)

data_abc_blocks <-
  bind_rows(data_grouped_abc_question_blocks_cleaned,
            data_interleafed_abc_question_blocks_cleaned) %>%
  split(.$case_id)

data_f_blocks <-
  data_f_question_blocks_cleaned %>%
  split(.$case_id)

# join de, abc and f question blocks
data_interleafed_grouped_combined <-
  pmap_dfr(
    list(de_block = data_de_blocks,
         abc_block = data_abc_blocks,
         f_block = data_f_blocks),
    ~ combine_all_blocks(..1, ..2, ..3)
  )




# ====== 9. add a1b1c1 as new rows =======
data_with_a1b1c1 <-
  data_interleafed_grouped_combined %>%
  split(.$case_id) %>%
  map_dfr(add_a1b1c1_as_new_rows)




# ====== 10. remove questions that the respondents did not answer ======
data_long <-
  map2_dfr(.x = data_with_a1b1c1 %>% split(.$case_id),
           .y = data_interim %>% split(.$nid),
           ~ clean_data_final_step(.x, .y))




# ====== 11. export data ======
save(data_interim,
     data_long,
     file = "./data/derived/data-dirty.RData")

