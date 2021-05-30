parse_cards <- function(x) {
  d <- scan(x, na.strings = c("Player", "1:", "Player", "2:"), quiet = TRUE)
  matrix(na.omit(d), ncol = 2)
}

stash <- function(a, b) {
  wins <- a > b
  c(rbind(a[wins], b[wins]))
}

stash_uneq <- function(a, b) {
  a_sub <- a[seq_along(b)]
  a <- c(a[-seq_along(b)], stash(a_sub, b))
  b <- stash(b, a_sub)
  list(a, b)
}

play <- function(m) {
  a <- m[, 1]; b <- m[, 2]
  while (length(a) != 0 && length(b) != 0) {
    if (length(a) > length(b)) {
      l <- stash_uneq(a, b)
      a <- l[[1]]; b <- l[[2]]
    } else {
      l <- stash_uneq(b, a)
      a <- l[[2]]; b <- l[[1]]
    }
  }
  if (length(b) == 0) winner <- a else winner <- b
  sum(winner * rev(seq_along(winner)))
}

# One
play(parse_cards("data/aoc_22"))
