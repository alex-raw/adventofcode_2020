handshake <- function(key, door, mod = 20201227L, max_tries = 1e8) {
  n <- 1L
  for (i in 1:max_tries) {
    n <- (n * 7L) %% mod
    if (n == key) break
  }
  n <- 1L
  for (j in 1:i)
    n <- (n * door) %% mod
  n
}

handshake(key = 6930903, door = 19716708)
