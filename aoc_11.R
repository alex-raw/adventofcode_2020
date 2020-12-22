# ----Part one
d <- readLines("data/aoc_11")

# pad edges, to avoid 'out of bounds'
d <- paste0(".....", d, ".....")
pad <- paste(rep(".", nchar(d[1])), collapse = "")
d <- c(pad, d, pad)

# turn into numeric matrix: 0, empty; NA, floor or edge
d <- strsplit(gsub("L", 0, d), "")
m <- lapply(d, function(x) suppressWarnings(as.numeric(x)))
m <- do.call(rbind, m)

neighbor_matrix <- function(matr, immediate = TRUE) {
  count <- function(xy) {
    x <- xy[1]; y <- xy[2]

    if (is.na(m[x, y])) {
      return(NA)                                     # ignore floor or edge

    } else {
      if (immediate == TRUE) {
        adj <- list(
          matr[x + 1, y],     matr[x - 1, y],        # down, up
          matr[x, y + 1],     matr[x, y - 1],        # right, left
          matr[x - 1, y + 1], matr[x - 1, y - 1],    # up: right, left
          matr[x + 1, y + 1], matr[x + 1, y - 1]     # down: right, left
        )
      } else {
        adj <- list(
          matr[seq(x + 1, nrow(matr)), y],
          matr[seq(x - 1, 1), y],
          matr[x, seq(y + 1, ncol(matr))],
          matr[x, seq(y - 1, 1)],
          diag(matr[seq(x - 1, 1), seq(y + 1, ncol(matr))]),
          diag(matr[seq(x - 1, 1), seq(y - 1, 1)]),
          diag(matr[seq(x + 1, nrow(matr)), seq(y + 1, ncol(matr))]),
          diag(matr[seq(x + 1, nrow(matr)), seq(y - 1, 1)])
        )
      }
    }
      first_seat <- sapply(adj, function(x) {
           x[suppressWarnings(min(which(
           !is.na(x))))] != 0
        }
      )
      return(sum(first_seat, na.rm = TRUE))
    }

  # count visible seats
  coord <- expand.grid(seq(nrow(matr)), seq(ncol(matr)))
  out <- matrix(apply(coord, 1, count), ncol = ncol(matr))

  # preserve sign (occupied seats)
  out[matr != 0 & !is.na(out)] <- out[matr != 0 & !is.na(out)] * -1
  return(out)
}

# simulation
simulate <- function(b, tolerance, immediate = TRUE) {
  ident <- i <- 0
  while (!ident) {
    a <- b

    b <- neighbor_matrix(b, immediate)
    b[b == 0 & !is.na(b)] <- -1
    b[b <= -tolerance & !is.na(b)] <- 0
    b[b > 0 & !is.na(b)] <- 0
    occupied <- sum(b < 0, na.rm = TRUE)
    i <- i + 1

    ident <- identical(a, b)
  }
  return(occupied)
}

# Part One
simulate(m, tolerance = 4, immediate = TRUE)
# Part Two
simulate(m, tolerance = 5, immediate = FALSE)
