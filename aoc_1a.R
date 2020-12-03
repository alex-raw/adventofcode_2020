x <- c(scan("data/aoc_1", quiet = TRUE))
prod(x[match(x, 2020 - x, nomatch = 0)])
