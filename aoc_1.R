x <- c(scan("data/aoc_1", quiet = TRUE))

# part one
prod(x[match(x, 2020 - x, nomatch = 0)])

# part two
matches <- list()
for (i in 2020 - x) {
  matches[[i]] <- x[match(x, i - x, nomatch = 0)]
}

prod(unique(unlist(matches)))
