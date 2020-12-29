# Part one and two
input <- c(15, 5, 1, 4, 7, 0)

play <- function(nums, goal) {
  nums  <- as.integer(nums)
  goal  <- as.integer(goal - 1L)

  pool  <- integer(goal)
  pool[nums + 1L] <- seq_along(nums)

  lnums <- length(nums)
  turns <- seq(lnums, goal)
  n <- nums[lnums]

  for (turn in turns) {
    i <- n + 1L
    if ((last <- pool[i]) == 0L) {
      n <- 0L
    } else {
      n <- turn - last
    }
    pool[i] <- turn
  }
  return(n)
}

# system.time({
play(input, 2020)
play(input, 30000000)
# })


# # {{{ Attempt three (video version)
# play <- function(nums, goal) {
#   pool <- integer(goal)
#   nums <- as.integer(nums)
#   goal <- as.integer(goal - 1L)
#   pool[nums + 1L] <- seq_along(nums)

#   turns <- seq(length(nums + 1L), goal)
#   n <- tail(nums, 1L)

#   for (turn in turns) {
#     last <- pool[n + 1L]
#     pool[n + 1L] <- turn
#     if (last == 0L) n <- 0L else n <- turn - last
#   }
#   return(n)
# }

# # Second attempt
# pool <- cbind(seq(length(input)), input, deparse.level = 0)
# turn <- i <- 7L
# n <- 0L

#   while (turn < goal) {
#     last <- n == pool[, 2]

#     if (sum(last) == 0) {
#       pool <- rbind(pool, c(turn, n))
#       n <- 0L
#       i <- i + 1L
#     } else {
#       n <- turn - pool[last, 1]
#       pool[last, 1] <- turn
#     }
#     turn <- turn + 1L
#   }

# # Attempt one
# play1 <- function(start_nums, goal) {
#   n <- c(start_nums, rep(NA, goal - length(start_nums)))
#   i <- length(start_nums)

#   while (i < goal) {
#     if (n[i] %in% n[1:(i - 1)]) {
#       n[i + 1] <- i - tail(which(n[-i] == n[i]), 1)
#     } else {
#       n[i + 1] <- 0L
#     }
#     i <- i + 1L
#   }
#   return(tail(n, 1))
# }

# system.time(play1(input, 2020))
