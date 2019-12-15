library(tidyverse)
all_pwds <- seq(172851, 675869)

good_pwds <- all_pwds %>%
    str_split("") %>%
    map(as.integer) %>%
    # check that there is at least one repeated digit
    keep(~ any(rle(.x)$lengths >= 2)) %>%
    # check that the digits are non-decreasing
    keep(~ all(diff(.x) >= 0))

# Part 1: how many good passwords are there?
length(good_pwds)

# Part 2: now need to make a small change, ensuring that there is an instance of
# a digit being repeated exactly twice.
best_pwds <- all_pwds %>%
    str_split("") %>%
    map(as.integer) %>%
    # check that there is one example of a digit appearing exactly twice in a
    # row
    keep(~ any(rle(.x)$lengths == 2)) %>%
    # check that the digits are non-decreasing
    keep(~ all(diff(.x) >= 0))

length(best_pwds)
