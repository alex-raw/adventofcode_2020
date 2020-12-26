d <- readLines("data/aoc_17")
d <- readLines("data/aoc_17_ex")
d <- matrix(nrow = nchar(d[1]), byrow = TRUE,
                as.logical(
                chartr("#.", "TF",
                unlist(strsplit(d, "")
                ))))

make_board <- function(m, iter) {
  lth <- nrow(m)

  pad1 <- matrix(FALSE, nrow = lth, ncol = iter)
  m <- cbind(pad1, m, pad1)

  pad2 <- matrix(FALSE, nrow = iter, ncol = (iter * 2) + lth)
  m <- rbind(pad2, m, pad2)

  arr <- array(FALSE, dim = rep(iter * 2 + lth, 3))
  arr[, , ceiling(nrow(arr) / 2)] <- m
  return(arr)
}

get_window <- function(n_dim, lth) {
  k <- 0L
  for (i in seq(n_dim) - 1L) {
    k <- c(k, k - lth ^ i, k + lth ^ i)
  }
  tail(k, -1)
}

w <- get_window(3, 3)

sum_neighbors <- function(x, w, data) {
  subs <- x + w
  subs <- subs[subs > 0]
  sum(data[subs])
}

sapply(seq_along(arr), sum_neighbors, w, arr)
