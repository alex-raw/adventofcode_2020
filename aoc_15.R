take_turns <- function(n, pool, turns) {
  for (turn in turns) {
    i <- n + 1L
    n <- if (last <- pool[i])
      turn - last else 0L
    pool[i] <- turn
  }
  n
}

play <- function(x, goal) {
  size <- length(x)
  pool <- integer(goal - 1)
  pool[x + 1L] <- seq_along(x)
  take_turns(n     = as.integer(x[size]),
             turns = seq(size, goal - 1),
             pool  = pool)
}

x <- c(15, 5, 1, 4, 7, 0)
c(part1 = play(x, 2020),
  part2 = play(x, 3e7))
