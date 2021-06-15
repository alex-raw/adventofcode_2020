parse_seats <- function(x) {
  pad <- paste(rep(".", nchar(x[1])), collapse = "")
  x <- c(pad, paste0(".....", x, "....."), pad) # to avoid 'out of bounds'
  x <- do.call(rbind, strsplit(x, ""))
  ifelse(x == "L", 0L, NA_integer_) # 0, empty; NA, floor or edge
}

count <- function(xy, m, immediate) {
  x <- xy[1];    y <- xy[2]
  nr <- nrow(m); nc <- ncol(m)

  if (is.na(m[x, y])) return(NA)
  adj <- if (immediate) list(
      m[x + 1, y],     m[x - 1, y],        # down, up
      m[x, y + 1],     m[x, y - 1],        # right, left
      m[x - 1, y + 1], m[x - 1, y - 1],    # up: right, left
      m[x + 1, y + 1], m[x + 1, y - 1])    # down: right, left
    else list(                             # embarrassing ...
      m[seq(x + 1, nr), y], m[seq(x - 1, 1), y],
      m[x, seq(y + 1, nc)], m[x, seq(y - 1, 1)],
      diag(m[seq(x - 1, 1), seq(y + 1, nc)]),
      diag(m[seq(x - 1, 1), seq(y - 1, 1)]),
      diag(m[seq(x + 1, nr), seq(y + 1, nc)]),
      diag(m[seq(x + 1, nr), seq(y - 1, 1)]))
  first_seat <- sapply(adj, \(x) x[which(!is.na(x))[1]] != 0)
  sum(first_seat, na.rm = TRUE)
}

neighbor_matrix <- function(m, immediate = TRUE) {
  coord <- expand.grid(seq(nrow(m)), seq(ncol(m)))
  out <- matrix(apply(coord, 1, count, m, immediate), nrow(m))
  ifelse(m != 0 & !is.na(out), -out, out) # preserve sign (occupied seats)
}

# simulation
simulate <- function(x, tolerance = 4, immediate = TRUE) {
  old <- 0
  while (!identical(old, x)) {
    old <- x
    x <- neighbor_matrix(x, immediate)
    is_seat <- !is.na(x)
    x[is_seat & x == 0] <- -1
    x[is_seat & (x > 0 | x <= -tolerance)] <- 0
  }
  sum(x < 0, na.rm = TRUE)
}

x <- parse_seats(readLines("data/aoc_11"))
c(part1 = simulate(x),
  part2 = simulate(x, tolerance = 5, immediate = FALSE))
