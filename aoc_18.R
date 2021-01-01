# Part one and two
`%+%` <- function(a, b) sum(a, b)
part_2  <- gsub("\\+", "%+%", readLines("data/aoc_18"))
part_1  <- gsub("\\*", "%*%", part_2)

solve <- function(x) print(sum(sapply(parse(text = x), eval)), 15)
solve(part_1)
solve(part_2)
