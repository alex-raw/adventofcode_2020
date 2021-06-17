handshake <- function(key, door, mod = 20201227L) {
  x <- y <- 1L; i <- 0
  while (!x == key) {
    x <- (x * 7L) %% mod
    i <- i + 1
  }
  for (j in 1:i) y <- (y * door) %% mod
  y
}

handshake(key = 6930903, door = 19716708)
