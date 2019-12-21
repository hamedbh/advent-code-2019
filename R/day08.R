library(tidyverse)
input <- scan("data/input08.txt",
              character()) %>%
    str_split("") %>%
    {
        .[[1]]
    } %>%
    as.integer() %>%
    matrix(ncol = 150, byrow = TRUE)
has_fewest_zeroes <- apply(input, 1, function(x) {
    sum(x == 0)
}
) %>%
    which.min()

visible_pixels <- map_int(seq_len(ncol(input)),
                          ~ min(which(input[, .x] != 2))) %>%
    imap_int(~ input[.x, .y])

matrix(visible_pixels, ncol = 6) %>%
    as.data.frame() %>%
    rowid_to_column() %>%
    gather(key = key, value = value, V1:V6) %>%
    mutate(key = gsub("V(.)", "\\1", key) %>% as.numeric()) %>%
    ggplot(aes(rowid, key, fill = as.factor(value))) +
    geom_tile() +
    coord_fixed() +
    scale_fill_viridis_d() +
    scale_y_reverse()
