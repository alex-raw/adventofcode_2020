parse_board <- function(x)
  chartr("#.", "TF", x) |>
    strsplit("") |> do.call(what = rbind) |>
    type.convert()

make_board <- function(m, k, iter) { # only does 3 or 4 dims...
  x <- nrow(m)
  dif <- x / 2

  arr <- array(FALSE, rep(iter * 2 + x, k))
  center <- median(seq(nrow(arr)))
  mid <- seq(ceiling(center - dif), floor(center + dif))

  if (k == 3) arr[mid, mid, center] <- m
  else        arr[mid, mid, center, center] <- m
  arr
}

window <- function(k, x) {
  # calculate vector indeces for n-dimensional array as 1d vector
  w <- 0L
  for (i in 0:(k - 1L)) w <- c(w, w - x^i, w + x^i)
  w[-1L]
}

neighbors <- function(arr, k) {
  w <- window(k, nrow(arr))
  i <- mapply(seq, w + 1, w + length(arr))
  i[i <= 0L] <- NA
  i
}

play_sim <- function(x, k, iter = 6L) {
  arr <- make_board(x, k, iter)
  i <- neighbors(arr, k)
  ln <- nrow(i)

  for (j in seq_len(iter)) {
    n <- rowSums(matrix(arr[i], ln), na.rm = TRUE)
    arr[arr]  <- n[arr] %in% c(2L, 3L)
    arr[!arr] <- n[!arr] == 3L
  }
  sum(arr, na.rm = TRUE)
}

x <- parse_board(readLines("data/aoc_17"))
c(part1 = play_sim(x, 3L),
  part2 = play_sim(x, 4L))
