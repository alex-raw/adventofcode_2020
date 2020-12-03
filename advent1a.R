x <- c(scan("day1_data"))
prod(x[match(x, 2020 - x, nomatch = 0)])
