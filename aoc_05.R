# interpret character string as sequence of functions
F <- L <- function(id) seq(min(id), floor(median(id)))
B <- R <- function(id) seq(ceiling(median(id)), max(id))
to_fun <- function(x)  strsplit(x, "")[[1]] |> lapply(match.fun)

decode <- function(funs, id) {
  for (fun in funs) id <- fun(id)
  id
}

seat_id <- function(d) {
  x <- gsub("R|L", "", d) |> to_fun() |> decode(0:127)
  y <- gsub("B|F", "", d) |> to_fun() |> decode(0:7)
  8 * x + y
}

d <- readLines("data/aoc_05")
seats <- sapply(d, seat_id)

max(seats)
sum(min(seats):max(seats)) - sum(seats)

# # binary, stupdid!
# to_bin <- function(x, a, b) strtoi(substr(x, a, b), 2L)
# x <- chartr("BRFL", "1100", readLines("data/aoc_05"))
# seats <- 8 * to_bin(x, 1, 7) + to_bin(x, 8, 10)
# c(part1 = max(seats),
#   part2 = sum(min(seats):max(seats)) - sum(seats))
