d <- scan("data/aoc_09")

# Part one
# Make offset grid
k <- 25
m <- matrix(d, ncol = k + 1, nrow = length(d) + 1)[1:1000, ]

find_error <- function(x) {
  length(intersect(x, x[k + 1] - x)) == 0
}

error <- m[which(apply(head(m, -k), 1, find_error)) + k]

m[which(apply(m, 1, cumsum) == error)]

# Part two
cums_error <- function(i) {
  v <- cumsum(d[i:length(d)])
  n <- which(v == error)

  if (length(n) > 0) d[i:(i + n - 1)]     # get both numbers
}

cons <- head(unlist(sapply(seq_along(d), cums_error)), -1)
min(cons) + max(cons)
