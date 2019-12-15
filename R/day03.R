library(tidyverse)

# The most important insight with this was to use tidyr::uncount() to 'expand'
# each of the commands (e.g. so that R1000 becomes 1000 rows), and then to use
# cumsum() to track the change in x and y coordinates. After that everything is
# just a join or two.
inputs <- readLines("data/input03.txt") %>%
    str_split(",") %>%
    imap(~ tibble(wire_id = .y,
                  cmd = .x) %>%
             mutate(cmd_id = row_number(),
                    direction = str_sub(cmd, 1L, 1L)) %>%
             mutate(len = as.integer(str_sub(cmd, 2L))) %>%
             uncount(len) %>%
             mutate(
                 total_steps = row_number(),
                 x_chg = case_when(
                     direction == "R" ~ 1L,
                     direction == "L" ~ -1L,
                     TRUE ~ 0L
                 ),
                 y_chg = case_when(
                     direction == "U" ~ 1L,
                     direction == "D" ~ -1L,
                     TRUE ~ 0L
                 )
             ) %>%
             mutate(x = cumsum(x_chg),
                    y = cumsum(y_chg)) %>%
             mutate(distance = abs(x) + abs(y)))

# Now split the list into the first and second wires
wire01 <- inputs[[1]]
wire02 <- inputs[[2]]

# Part 1: what is the shortest Manhattan distance from an intersection to the
# origin?
wire01 %>%
    inner_join(wire02,
               by = c("x", "y")) %>%
    top_n(1, -distance.x) %>%
    pull(distance.x)

# Part 2: which intersection happens after the fewest steps?
wire01 %>%
    inner_join(wire02,
               by = c("x", "y")) %>%
    mutate(total_steps = total_steps.x + total_steps.y) %>%
    top_n(1, -total_steps) %>%
    pull(total_steps)
