x <- c(scan("day1_data"))

lol <- list()
for (i in 2020 - x) {
  lol[[i]] <- x[match(x, i - x, nomatch = 0)]
}

prod(unique(unlist(lol)))
