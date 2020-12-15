df <- scan("data/aoc_03", "character", quiet = TRUE)

# Part 1
slope <- c(3, 1)

check_tree <- function(x, y) {
  substr(x, y, y) == "#"
}

count_trees <- function(slope, x) {
  right <- slope[[1]]
  down <- slope[[2]]

  # make enough grid to get to the bottom
  width_factor <- ceiling(right * length(x) / nchar(x[1]))

  complete <- function(x) {
    paste(replicate(width_factor, x), collapse = "")
  }

  full <- sapply(x, complete)

  # set up coordinates
  coor_x <- seq(1, right * length(full), by = right)
  coor_y <- seq(1, length(full), by = down)

  sum(check_tree(full[coor_y], coor_x))
}

count_trees(c(3, 1), df)

# Part 2
slope <- list(
  c(1, 1),
  c(3, 1),
  c(5, 1),
  c(7, 1),
  c(1, 2)
)

trees <- sapply(slope, count_trees, df)
prod(trees)
