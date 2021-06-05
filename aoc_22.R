parse_cards <- function(x) {
  d <- scan(x, comment.char = "P", quiet = TRUE) |> matrix(ncol = 2)
  list(d[, 1], d[, 2])
}

combat <- function(a, b) {
  n   <- seq_along(b)
  ans <- cbind(a[n], b)
  win <- ans[, 1] > ans[, 2]
  list(a = c(a[-n], t(ans[win, ])),
       b = c(t(ans[!win, 2:1])))
}

play <- function(x) repeat {
  n <- lengths(x)
  if (any(n == 0)) return(x[[i]])
  i <- which.max(n)
  x <- combat(x[[i]], x[[-i]])
}

solve <- function(x) sum(x * length(x):1)

# One
parse_cards("data/aoc_22") |> play() |> solve()
