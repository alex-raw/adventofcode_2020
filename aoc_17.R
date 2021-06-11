d <- chartr("#.", "TF", readLines("data/aoc_17"))
d <- do.call(rbind, strsplit(d, ""))
mode(d) <- "logical"

# parse_board <- function(path) readLines(path) |>
#   strsplit("") |>
#   do.call(what = rbind) |>
#   chartr(old = "#.", new = "TF") |>
#   type.convert()

# Part 1 and 2 ---------------------------------
make_board <- function(m, n_dim, iter) {
  # Pre-allocate array big enough to fit all iterations
  # only does 3 or 4 dims...
  lth <- nrow(m)
  dif <- lth / 2

  arr <- array(FALSE, rep(iter * 2 + lth, n_dim))
  center <- median(seq(nrow(arr)))
  mid <- seq(ceiling(center - dif), floor(center + dif))

  if (n_dim == 3) {
    arr[mid, mid, center] <- m
  } else {
    arr[mid, mid, center, center] <- m
  }

  return(arr)
}

get_window <- function(n_dim, lth) {
  # calculate vector indeces for n-dimensional array as 1d vector
  k <- 0L
  for (i in seq(n_dim) - 1L) {
    k <- c(k, k - lth ^ i, k + lth ^ i)
  }
  tail(k, -1)
}

sum_neighbors <- function(x, w, arr) {
  # sum up neighboring sells of `x` in window `w` in array `arr`
  i <- x + w
  i <- i[i > 0]
  sum(arr[i], na.rm = TRUE)
}

play_sim <- function(x, n_dim, iter = 6) {
  arr <- make_board(x, n_dim, iter)
  w   <- get_window(n_dim, nrow(arr))
  arr <- as.vector(arr)

  for (i in seq_len(iter)) {
    nbs <- sapply(seq_along(arr), sum_neighbors, w, arr)
    arr[arr]  <- nbs[arr] %in% c(2, 3)
    arr[!arr] <- nbs[!arr] == 3
  }

  sum(arr, na.rm = TRUE)
}

play_sim(d, 3)
play_sim(d, 4)

parse_input <- function(path) {
  d <- readLines(path)
  d <- unlist(strsplit(d, ""))
    chartr(old = "#.", new = "TF") |> as.logical() |>
    matrix(nrow = nchar(d[1]), byrow = TRUE)
}

