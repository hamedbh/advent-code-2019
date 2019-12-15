library(tidyverse)
original_input <- readLines("data/input05.txt") %>%
    str_split(",", simplify = TRUE) %>%
    as.integer()
original_input
