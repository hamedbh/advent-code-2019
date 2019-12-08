# Day 1: to get fuel divide the weight of the module by 3, round down, and then
# subtract two. Sum that to get the answer.
library(tidyverse)
input <- readLines("data/input01.txt") %>%
    as.integer()
module_fuel <- function(module) {
    (module %/% 3) - 2
}
sum(module_fuel(input))
# 3380731
