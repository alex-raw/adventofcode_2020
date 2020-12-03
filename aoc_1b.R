x <- c(scan("data/aoc_1", quiet = TRUE))

matches <- list()
for (i in 2020 - x) {
  matches[[i]] <- x[match(x, i - x, nomatch = 0)]
}

prod(unique(unlist(matches)))
