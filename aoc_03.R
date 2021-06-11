parse_grid <- function(path) readLines(path) |>
  strsplit("") |> do.call(what = rbind) |>
  chartr(old = "#.", new = "TF") |> type.convert()

get_xy <- function(k, mod, n) {
  ans <- seq(1, n, by = k) %% mod
  ans[ans == 0] <- mod; ans
}

count_trees <- function(m, x, y) sum(m[
  x = get_xy(x, ncol(m), x * ncol(m)),
  y = get_xy(y, nrow(m), ncol(m))
])

# Part 1
m <- parse_grid("data/aoc_03")
count_trees(m, 3, 1)

# Part 2
slopes <- rbind(c(1, 1), c(3, 1), c(5, 1), c(7, 1), c(1, 2))
prod(mapply(count_trees, slopes[, 1], slopes[, 2], MoreArgs = list(m = m)))
