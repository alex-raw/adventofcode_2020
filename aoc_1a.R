x <- c(scan("aoc_1_data", quiet = TRUE))
prod(x[match(x, 2020 - x, nomatch = 0)])
