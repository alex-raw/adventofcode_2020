x <- c(scan("day1_data", quiet = TRUE))

matches <- list()
for (i in 2020 - x) {
  matches[[i]] <- x[match(x, i - x, nomatch = 0)]
}

prod(unique(unlist(matches)))
