# Day 1: to get fuel divide the weight of the module by 3, round down, and then
# subtract two. Sum that to get the answer.
library(tidyverse)
input <- readLines("data/input01.txt") %>%
    as.integer()
calc_fuel <- function(weight) {
    (weight %/% 3L) - 2L
}
sum(calc_fuel(input))
# 3380731

# Part 2: now need to recursively calculate fuel cost for the fuel until that
# amount is non-positive, at which point can stop
fuel_for_fuel <- function(weight) {
    results <- calc_fuel(weight)
    while (calc_fuel(results[length(results)]) > 0) {
        results <- c(results, calc_fuel(results[length(results)]))
    }
    sum(results)
}

map_int(input, fuel_for_fuel) %>%
    sum()
# 5068210
