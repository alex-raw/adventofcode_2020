d <- scan("data/aoc_5", "character", quiet = TRUE)

# ids of rows and columns
rw <- 0:127
cl <- 0:7

# Row
F <- L <- function(id) {
  id <<- seq.int(min(id), floor(median(id)))
}

B <- R <- function(id) {
  id <<- seq.int(ceiling(median(id)), max(id))
}

# interpret character string as sequence of functions
decode <- function(code) {
  funs <- strsplit(code, split = "")
  for (i in funs[[1]]) {
    do.call(i, list(id))
  }
  return(id)
}

# Find seat id
seat_id <- function(code) {
  # row number
  id <<- rw
  funs <- gsub("R|L", "", code)
  x <- decode(funs)

  # column number
  id <<- cl
  funs <- gsub("B|F", "", code)
  y <- decode(funs)

  (x * 8) + y
}

seats <- sapply(d, seat_id)
max(seats)

# Part two
sum(min(seats):max(seats)) - sum(seats)









# binary, stupdid!
b <- gsub("B|R", "1", gsub("F|L", "0", d))
b <- sapply(list(substr(b, 1, 7), substr(b, 8, 10)), strtoi, 2L)
seats <- b1[, 1] * 8 + b1[, 2]

max(seats)
sum(min(seats):max(seats)) - sum(seats)
