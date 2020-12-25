input <- c(15, 5, 1, 4, 7, 0)

# Part one and two; exec time ~4s
play <- function(nums, goal) {
  pool <- integer(goal)
  pool[nums + 1] <- seq_along(nums)

  turns <- seq(7, goal - 1)
  n <- tail(nums, 1)

  for (turn in turns) {
    last <- pool[n + 1]
    pool[n + 1] <- turn
    if (last == 0) n <- 0 else n <- turn - last
  }
  return(n)
}

play(input, 2020)
play(input, 30000000)




# {{{ Attempt one

play1 <- function(start_nums, goal) {
  n <- c(start_nums, rep(NA, goal - length(start_nums)))
  i <- length(start_nums)

  while (i < goal) {
    if (n[i] %in% n[1:(i - 1)]) {
      n[i + 1] <- i - tail(which(n[-i] == n[i]), 1)
    } else {
      n[i + 1] <- 0L
    }
    i <- i + 1L
  }
  return(tail(n, 1))
}

system.time(play1(input, 2020))



















# Second attempt
pool <- cbind(seq(length(input)), input, deparse.level = 0)
turn <- i <- 7L
n <- 0L

  while (turn < goal) {
    last <- n == pool[, 2]

    if (sum(last) == 0) {
      pool <- rbind(pool, c(turn, n))
      n <- 0L
      i <- i + 1L
    } else {
      n <- turn - pool[last, 1]
      pool[last, 1] <- turn
    }
    turn <- turn + 1L
  }
