d <- readLines("data/aoc_06") |> paste(collapse = "\n") |>
  strsplit("\n\n") |> unlist() |>        # vector of groups
  strsplit("\n") |> lapply(strsplit, "") # list of character vectors per group

yes <- function(x, fun) sum(lengths(lapply(x, fun)))
c(part1 = yes(d, \(x) unique(unlist(x))),
  part2 = yes(d, \(x) Reduce(intersect, x)))
