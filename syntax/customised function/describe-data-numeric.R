library(tidyverse)

describe_data_numeric <- 
  function(.vector) {
    tibble(min = min(.vector, na.rm = T),
           max = max(.vector, na.rm = T),
           median = median(.vector, na.rm = T),
           mean = mean(.vector, na.rm = T),
           sd = sd(.vector, na.rm = T),
           n = length(.vector),
           na_n = sum(.vector %in% NA),
           na_prop = na_n / n * 100)
  }
