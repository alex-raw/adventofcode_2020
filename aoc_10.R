# Part 1: readable
adapters <- scan("data/aoc_10")

outlet <- 0
built_in <- max(adapters) + 3
chain <- sort(c(outlet, adapters, built_in))

prod(table(diff(chain)))

# Part 2: after reading about the procedure, at least implemented myself :(

delta <- paste0(diff(chain), collapse = "")
groups <- table(strsplit(delta, "3"))
result <- 2 ^ groups["11"] * 4 ^ groups["111"] * 7 ^ groups["1111"]

print(result, digits = 15)

# Part 1: one-liner
# prod(table(diff(sort(c(0, scan("data/aoc_10"), max(scan("data/aoc_10")) + 3)))))

# #--- Part 2: Fail 1; brute force
# # skippable numbers
# skip_1 <- chain[c(FALSE, diff(chain,  2) <= 3, FALSE)]

# # get all combinations of skippable numbers of all lengths
# get_combn <- lapply(seq_along(skip_1), function(x) combn(skip_1, x))

# # test whether taking out numbers violates rule
# test <- function(set) {
#   max(diff(chain[!chain %in% set])) <= 3
# }

# # all possible combinations, +1 is the original
# sum(sapply(lapply(get_combn, function(x) apply(x, 2, test)), sum)) + 1

# # Part 2: Fail 2; gave up after realising that I couldn't reduce combinations enough
# # skippable concecutives, among which the FALSE combinations must be
# skip_2 <- chain[c(FALSE, diff(chain,  3) <= 3, FALSE)]
# mat <- which(as.matrix(dist(which(chain %in% skip_1))) == 1, arr.ind = TRUE)
# new <- rbind(cons[mat[, 1]], cons[mat[, 2]])
# lol <- new[, c(TRUE, FALSE)]
