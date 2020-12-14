# ----Part one
d <- readLines("data/aoc_11_ex")

# pad edges, to avoid 'out of bounds errors'
d <- paste0(".....", d, ".....")
pad <- rep(c(paste(rep(".", nchar(d[1])), collapse = "")), 5)
d <- c(pad, d, pad)
d <- strsplit(gsub("L", 0, d), "")

# turn into numeric matrix: 0, empty; 1, occupied; NA, floor
m <- lapply(d, function(x) suppressWarnings(as.numeric(x)))
m <- do.call(rbind, m)

neighbor_matrix <- function(matr, immediate = TRUE) {

  count <- function(xy, matr) {
    x <- xy[1]; y <- xy[2]

    if (is.na(m[x, y])) {                            # ignore floor or edge
      return(NA)
    } else {
      if (immediate == TRUE) {
        adj <- list(
          matr[x + 1, y],     matr[x - 1, y],        # down, up
          matr[x, y + 1],     matr[x, y - 1],        # 1ight, left
          matr[x - 1, y + 1], matr[x - 1, y - 1],    # up: 1ight, left
          matr[x + 1, y + 1], matr[x + 1, y - 1]     # down: 1ight, left
        )
      } else {
        adj <- list( # TODO: rewrite!!
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
                             n <- x[suppressWarnings( min(which(!is.na(x))))]
                             n != 0# & n != Inf
        }
      )
      return(sum(first_seat, na.rm = TRUE))
    }

  coord <- expand.grid(seq(nrow(matr)), seq(ncol(matr)))
  out <- matrix(apply(coord, 1, count, matr), ncol = ncol(matr))

  # preserve sign of input
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





# attempt to auto-create directions
# sgn <- c(" + r", " - r", "")
# xx <- mapply(paste0, "matr[x", sgn, ", ", " y")
# sets <- c(sapply(xx, function(x) mapply(paste0, x, sgn, "]")))
# test <- list()
# for (i in sets) test[i] <- eval(parse(text = i))
# test

# neighbor_matrix <- function(matr, r = 1) {

#   count <- function(xy, matr) {
#     x <- xy[1]; y <- xy[2]
#     r <- 1:r

#     if (is.na(m[x, y])) {                            # ignore floor or edge
#       return(NA)
#     } else {
#       adj <- list(
#         matr[x + r, y],     matr[x - r, y],          # down, up
#         matr[x, y + r],     matr[x, y - r],          # right, left
#         matr[x + r, y + r], matr[x + r, y - r],      # right + down, up

#         # # if r > 1
#         # rd <- diag(matr[x + r, y + r]),    # right down
#         # ru <- diag(matr[x + r, y - r]),    # right up
#         # ld <- diag(matr[x - r, y + r]),    # left down
#         # lu <- diag(matr[x - r, y - r])     # left upr[x - r, y + r], matr[x - r, y - r]       # left + down, up
#       )

#       first_seat <- sapply(adj, function(x) x[suppressWarnings(
#                                               min(which(!is.na(x))))] != 0)
#       return(sum(first_seat, na.rm = TRUE))
#     }
#   }
