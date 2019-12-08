library(tidyverse)
original_input <- readLines("data/input02.txt") %>%
    str_split(",", simplify = TRUE) %>%
    as.integer()
input <- original_input
# Replace these as directed before beginning processing
input[2] <- 12L
input[3] <- 2L

i <- 1L

while (i <= length(input)) {
    if (input[i] == 1) {
        input[input[i + 3] + 1] <-
            input[input[i + 1] + 1] + input[input[i + 2] + 1]
        i <- i + 4L
    } else if (input[i] == 2) {
        input[input[i + 3] + 1] <-
            input[input[i + 1] + 1] * input[input[i + 2] + 1]
        i <- i + 4L
    } else if (input[i] == 99) {
        break
    }
}
input[1]
# 9581917

# Now need to re-run this but changing the values that are inserted at
# positions 1 and 2, called noun and verb respectively
results <- crossing(noun = seq_len(100), verb = seq_len(100)) %>%
    mutate(output = pmap_int(.,
                             function(noun, verb) {
                                 input <- original_input
                                 # Replace these as directed before beginning processing
                                 input[2] <- noun
                                 input[3] <- verb

                                 i <- 1L

                                 while (i <= length(input)) {
                                     if (input[i] == 1) {
                                         input[input[i + 3] + 1] <-
                                             input[input[i + 1] + 1] +
                                             input[input[i + 2] + 1]
                                         i <- i + 4L
                                     } else if (input[i] == 2) {
                                         input[input[i + 3] + 1] <-
                                             input[input[i + 1] + 1] *
                                             input[input[i + 2] + 1]
                                         i <- i + 4L
                                     } else if (input[i] == 99) {
                                         break
                                     }
                                 }
                                 input[1]
                             }))

# Get the combination of noun and verb that gives the right answer, do the
# arithmetic to collapse that to one number for the solution
results %>%
    filter(output == 19690720) %>%
    mutate(answer = 100 * noun + verb)
