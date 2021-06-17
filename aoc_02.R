parse_input <- function(x) {
  read.table(text = gsub("-|: ", " ", x),
  col.names = c("a", "b", "char", "pass"))
}

with(parse_input(readLines("data/aoc_02")), {
  pass <- strsplit(pass, "")
  n <- mapply(\(x, y) sum(x == y), char, strsplit(pass, ""))
  c(part1 = sum(n >= a & n <= b),
    part2 = sum(xor(pass[a] == char,
                    pass[b] == char)))
})
