df <- scan("data/aoc_3", "character")

check_tree <- function(x, y) {
  substr(x, y, y) == "#"
}

count_trees <- function(slopes, x) {
  right <- slopes[[1]]
  down <- slopes[[2]]
  coor_x <- seq(1, right * length(full) + 100, by = right)
  coor_y <- seq(1, length(full), by = down)

  width_factor <- ceiling((3 * length(x)) / nchar(x[1]))

  complete <- function(x) {
    paste(replicate(width_factor, x), collapse = "")
  }

  full <- sapply(x, complete)

  sum(check.tree(full[coor_y], coor_x))
}

count_trees(c(3, 1), df)

slopes <- list(
  c(1, 1),
  c(3, 1),
  c(5, 1),
  c(7, 1),
  c(1, 2)
)

coor_y <- seq(1, length(full), by = 2)
coor_y

df[coor_y]

count_trees(c(1,2), df)

sapply(slopes, count_trees, x = df)
