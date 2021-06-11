parse_input <- function(path) readLines(path) |>
  strsplit("-| |(: )") |> do.call(what = rbind.data.frame) |>
  `colnames<-`(c("a", "b", "char", "pass")) |> type.convert()

with(parse_input("data/aoc_02"), {
  n <- mapply(\(x, y) sum(x == y), char, strsplit(pass, ""))
  c(part1 = sum(n >= a & n <= b),
    part2 = sum(xor(substr(pass, a, a) == char,
                    substr(pass, b, b) == char)))
})
