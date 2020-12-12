# Part 1 one-liner
# prod(table(diff(sort(c(0, scan("data/aoc_10"), max(scan("data/aoc_10")) + 3)))))

# adapters <- c(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
adapters <- c(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
# adapters <- scan("data/aoc_10"), quiet = TRUE)

outlet <- 0
built_in <- max(adapters) + 3
chain <- c(outlet, adapters, built_in)

length(chain)

# how many adapters can be skipped
sum(diff(sort(chain), 2) <= 3)

# which adapters can be skipped
diff(sort(chain))
val <- length(chain[c(FALSE, diff(sort(chain), 2) <= 3, FALSE)])


n_comb <- function(n) {
  k <- seq(n)

  comb <- function(k) {
    (factorial(n) / (factorial(k) * factorial(n - k)))
  }

  sum(sapply(k, comb)) + 1
}

n_comb(val)
