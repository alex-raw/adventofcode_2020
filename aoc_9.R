d <- scan("data/aoc_9", quiet = TRUE)

# Part one
# Make offset grid with k columns; FIXME: why warnings?
k <- 25

for (i in seq(k)) {
   d <- cbind(d, c(d[-1:-i], d[1:i]))
}

find_error <- function(x) {
  length(intersect(x, x[k + 1] - x)) == 0
}

(error <- d[which(apply(head(d, -k), 1, find_error)) + k])

# Part two

d <- d[, 1]

cum <- function(i) {
  v <- cumsum(d[i:length(d)])
  n <- which(v == error)

  if (length(n) > 0) {
    d[i:(i + n - 1)]
  }
}

cons <- head(unlist(sapply(seq_along(d), cum)), -1)
min(cons) + max(cons)
