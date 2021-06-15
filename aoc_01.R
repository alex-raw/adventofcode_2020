solve <- function(x, n)
  prod(intersect(x, outer(2020 - (n - 2) * x, x, "-")))

x <- scan("data/aoc_01", quiet = TRUE)
c(part1 = solve(x, 2), part2 = solve(x, 3))

# slow oneliner; read stdin; 45 chars
# combn(scan(),3)->.;prod(.[,colSums(.)==2020])
