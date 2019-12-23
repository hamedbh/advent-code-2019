library(tidyverse)
input <- scan("data/input10.txt",
     character()) %>%
    as_tibble() %>%
    rowid_to_column("y_coord") %>%
    mutate(each_value = map(value,
                            ~ str_split(.x, ""))) %>%
    unnest(cols = c(each_value)) %>%
    unnest(cols = c(each_value)) %>%
    group_by(y_coord) %>%
    mutate(x_coord = row_number()) %>%
    ungroup() %>%
    transmute(x_coord,
              y_coord,
              has_asteroid = if_else(each_value == "#",
                                     1L,
                                     0L))




as.matrix() %>%
    apply(1,
          function(x) {
              str_replace_all(x,
                              "\\.",
                              "0") %>%
                  str_replace_all("\\#",
                                  "1")
          }) %>%
    as_tibble()
input
