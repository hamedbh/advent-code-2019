# Took this from Colin Fay's solution: I was already so far behind, and found 
# the intcode stuff so annoying, that I just wanted to move on to the next one.

vec <- scan("data/input05.txt", numeric(), sep = ",")
library(magrittr)
library(zeallot)
parse_opcode <- function(vec, pos){
    code <- vec[pos]
    op_code <- stringr::str_pad(
        as.character(code), 
        5, 
        side = "left", "0"
    ) %>% strsplit(split = "")
    op_code <- op_code[[1]] %>% 
        setNames(letters[1:5])
    list(
        op_code = paste0(
            op_code[4:5], 
            collapse = ""
        ) %>% as.numeric(), 
        c = op_code["c"],
        b = op_code["b"], 
        a = op_code["a"]
    ) %>%
        one_two_three(
            vec, 
            pos
        )
}

one_two_three <- function(res, vec, pos){
    if (res$c == "0"){
        one <- vec[vec[pos + 1] + 1]
    } else {
        one <- vec[pos + 1] 
    } 
    if (res$b == "0"){
        two <- vec[vec[pos + 2] + 1]
    } else {
        two <- vec[pos + 2] 
    } 
    if (res$a == "0"){
        three <- vec[pos + 3] + 1
    } else {
        three <- vec[pos + 3]
    } 
    list(
        res$op_code,
        one, 
        two, 
        three
    )
}

prog <- function(vec, ipt){
    pos <- 1
    while (pos < length(vec)) {
        
        c(op_code, one, two, three) %<-%  parse_opcode( vec,  pos )
        
        if (op_code == 99) break
        
        if (op_code %in% 1:2){
            
            if (op_code == 1) fun <- `+`
            if (op_code == 2) fun <- `*`
            vec[three] <- fun(one, two)
            pos <- pos + 4
        } else if (op_code == 3){
            vec[ vec[pos + 1] + 1 ] <- ipt
            pos <- pos + 2
        } else if (op_code == 4){
            print(vec[vec[pos + 1] + 1])
            pos <- pos + 2
        } else if (op_code == 5){
            if (one != 0){
                pos <- two + 1
            } else {
                pos <- pos + 3
            }
        } else if (op_code == 6){
            if (one == 0){
                pos <- two + 1
            } else {
                pos <- pos + 3
            }
        } else if (op_code == 7){
            if (one < two){
                vec[three] <- 1 
            } else {
                vec[three] <- 0 
            }
            pos <- pos + 4
        } else if (op_code == 8){
            if (one == two){
                vec[three] <- 1 
            } else {
                vec[three] <- 0 
            }
            pos <- pos + 4
        }
    }
}
# Part 1 answer
prog(vec, 1)
# Part 2 answer
prog(vec, 5)

