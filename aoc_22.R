parse_cards <- function(x) {
  d <- scan(x, comment.char = "P") |> matrix(ncol = 2)
  list(d[, 1], d[, 2])
}

combat <- function(a, b) {
  wins <- a > b
  rbind(a[wins], b[wins])
}

stash <- function(a, b) {
  nb <- seq_along(b)
  a1 <- a[nb]; a2 <- a[-nb]
  list(c(a2, combat(a1, b)), combat(b, a1))
}

play <- function(m) {
  repeat {
    n <- lengths(m)
    if (any(n == 0)) break
    i <- which.max(n)
    m <- stash(m[[i]], m[[-i]])
  }

  winner <- m[[i]]
  sum(winner * rev(seq_along(winner)))
}

# One
play(parse_cards("data/aoc_22"))
