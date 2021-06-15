tribonacci <- function(diffs) {
  groups <- paste0(diffs, collapse = "") |> strsplit("3") |> table()
  2 ^ groups[["11"]] * 4 ^ groups[["111"]] * 7 ^ groups[["1111"]]
}

x <- scan("data/aoc_10")
diffs <- diff(sort(c(0, x, max(x) + 3)))

c(part1 = prod(table(diffs)),
  part2 = tribonacci(diffs) # fail: had to look it up
) |> print(15)

# # part 1; stdin; 50 chars
# scan()->.;prod(table(diff(sort(c(0,.,max(.)+3)))))
