# part one
split_number <- function(x)
  as.character(x) |> strsplit("") |>
  unlist() |> as.integer()

move <- function(x) {
  current <- x[1L]
  pick_up <- x[2:4]
  rest <- x[-(1:4)]

  dest <- current - 1L
  while (any(dest == pick_up))
    dest <- dest - 1L
  if (!dest) dest <- max(rest)

  id <- seq_len(which(rest == dest))
  c(rest[id], pick_up, rest[-id], current)
}

play <- function(x, n, part_two = FALSE) {
  for (i in seq_len(n))
    x <- move(x)
  id_one <- which.min(x)

  if (part_two)
    prod(x[id_one + 1:2])
  else {
    ids <- seq_len(id_one)
    c(x[-ids], x[ids - 1])
  }
}

# test_id <- split_number(389125467)
input <- split_number(247819356)
input2 <- c(input, seq(max(input) + 1, 1e6))

play(input, 100) |> paste0(collapse = "")
# ca. 36 hours... meh
play(input2, 1e7, part_two = TRUE)
