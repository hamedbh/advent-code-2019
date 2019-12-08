library(tidyverse)
# input <- readLines("data/input03.txt") %>%
#     str_split(",") %>%
#     imap_dfr(~ tibble(wire = .y,
#                       value = .x)) %>%
#     group_by(wire) %>%
#     mutate(id = row_number()) %>%
#     ungroup() %>%
#     select(wire, id, value) %>%
#     mutate(all_pts = vector("list", nrow(.)),
#            x = vector("list", nrow(.)),
#            y = vector("list", nrow(.)))
scan("data/input03.txt", what = character(), sep = "\n")
input <- readLines("data/input03.txt") %>%
    str_split(",")
add_wire_points <- function(x, y, cmd) {
    direction <- str_sub(cmd, 1, 1)
    distance <- str_sub(cmd, 2) %>%
        as.integer()
    out_x <- integer(distance)
    out_y <- integer(distance)

    if (direction == "U") {
        x <- c(x, rep(x, distance))
        y <- c(y, seq(y + 1, y + distance))
    } else if (direction == "D") {
        x <- c(x, rep(x, distance))
        y <- c(y, seq(y - 1, y - distance))
    } else if (direction == "R") {
        y <- c(y, rep(y, distance))
        x <- c(x, seq(x + 1, x + distance))
    } else if (direction == "L") {
        y <- c(y, rep(y, distance))
        x <- c(x, seq(x - 1, x - distance))
    }

    list(x = x,
         y = y)
}


add_wire_points(0, 0, input[[1]][[1]])

# input[input$id == 1, ] <- list(0)
#
# for (wire in c(1, 2)) {
#     for (id in seq_len(max(input$id))) {
#         input
#     }
# }
#
