# Make offset grid using recycling
find_error <- function(x, k = 25) {
  ln <- length(x)
  nums <- x[-seq_len(k)]
  m <- matrix(x, ln + 1, ln - k)[1:k, ]
  for (i in seq_along(nums)) {
    n <- nums[i]
    w <- m[, i]
    if (!any(w %in% (n - w))) return(n)
  }
}

find_contiguous <- function(x, error) {
  for (i in seq_along(x)) {
    id <- match(error, cumsum(x[-(1:i)]), 0L)
    if (id) return(x[seq(id, length = i)])
  }
}

x <- scan("data/aoc_09")
c(part1 = error <- find_error(x, 25),
  part2 = find_contiguous(x, error) |> range() |> sum())
