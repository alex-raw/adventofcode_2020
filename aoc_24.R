# parsing: replacing east (e) and west (w) with two character
# keys in order to simply split at every two characters
subtile <- function(x, y) gsub(x = x,
    paste0("(?<![sn])", y),
    paste0("x", y),
    perl = TRUE
)

split_tiles <- function(x)
  subtile(x, "e") |> subtile("w") |>
  strsplit("(?<=..)", perl = TRUE)

# global variable with relative hex coordinates
neighbors <- rbind(
  ne = c( 0, -1, 1), nw = c(1, -1,  0),
  se = c(-1,  1, 0), sw = c(0,  1, -1),
  xe = c(-1,  0, 1), xw = c(1,  0, -1)
)

first_flip <- function(x) {
  find_target <- \(x) colSums(neighbors[x, ])
  input <- lapply(split_tiles(x), find_target)

  tile <- unique(input)
  black <- !duplicated(input, fromLast = TRUE)[seq_along(tile)]

  list(tile = tile, black = black)
}

# Part 1
coordinates <- first_flip(readLines("data/aoc_24_ex"))
sum(coordinates$black)

# Part2
# keep list structure for matching with `%in%`
match_neighbors <- function(rel_coord, xyz)
  lapply(xyz, "+", rel_coord) %in% xyz

count_neighbors <- function(xyz)
  apply(neighbors, 1, match_neighbors, xyz) |> rowSums()

flip <- function(x) {
  n <- count_neighbors(x$tile)
  x$black[!x$black & (n == 2)] <- TRUE
  x$black[x$black & (n == 0 | n > 2)] <- FALSE
  x
  # ... have to consider the entire "floor"/hexgrid at arbitrary size
  # don't feel like another conway ...
}

x <- coordinates
for (i in 1:100) x <- flip(x)
sum(x$black)
