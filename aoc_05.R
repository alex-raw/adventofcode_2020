# interpret character string as sequence of functions
F <- L <- \(id) seq(min(id), floor(median(id)))
B <- R <- \(id) seq(ceiling(median(id)), max(id))
to_fun <- \(x)  strsplit(x, "")[[1]] |> lapply(match.fun)

decode <- function(x, id) {
  for (fun in to_fun(x)) id <- fun(id)
  id
}

seat_id <- function(d) {
  x <- gsub("R|L", "", d) |> decode(0:127)
  y <- gsub("B|F", "", d) |> decode(0:7)
  8 * x + y
}

seats <- readLines("data/aoc_05") |> sapply(seat_id)
c(part1 = max(seats),
  part2 = sum(min(seats):max(seats)) - sum(seats))

# # binary, stupdid!
# to_bin <- function(x, a, b) strtoi(substr(x, a, b), 2L)
# x <- chartr("BRFL", "1100", readLines("data/aoc_05"))
# seats <- 8 * to_bin(x, 1, 7) + to_bin(x, 8, 10)
# c(part1 = max(seats),
#   part2 = sum(min(seats):max(seats)) - sum(seats))

Reduce(Funcall, , id)
