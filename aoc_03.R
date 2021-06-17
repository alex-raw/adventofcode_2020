parse_grid <- function(x)
  chartr("#.", "TF", x) |> strsplit("") |>
  do.call(what = rbind) |> type.convert()

get_xy <- function(k, mod, n) {
  ans <- seq(1, n, by = k) %% mod
  ans[ans == 0] <- mod; ans
}

count_trees <- function(m, x, y)
  sum(m[x = get_xy(x, ncol(m), x * ncol(m)),
        y = get_xy(y, nrow(m), ncol(m))])

m <- parse_grid(readLines("data/aoc_03"))
slopes <- rbind(c(1, 1), c(3, 1), c(5, 1), c(7, 1), c(1, 2))

c(part1 = count_trees(m, 3, 1),
  part2 = prod(mapply(count_trees, slopes[, 1], slopes[, 2],
                      MoreArgs = list(m = m))))
