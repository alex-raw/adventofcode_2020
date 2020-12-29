d <- readLines("data/aoc_18")

# Part one and two
`%sum%`  <- function(a, b) sum(a, b)
evaluate <- function(v) sapply(v, function(x) eval(parse(text = x)))
solve    <- function(x) print(sum(evaluate(x)), 15)

part_1 <- gsub("\\*", "%*%", gsub("\\+", "%sum%", d))
part_2 <- gsub("\\+", "%sum%", d)

solve(part_1)
solve(part_2)
