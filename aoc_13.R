get_waiting_times <- function(ids, stamp) {
  ids <- na.omit(ids)
  waiting <- Map(seq, 0L - stamp, ids, by = ids) |>
    vapply(\(x) min(x[x > 0]), 1L)
  ids[which.min(waiting)] * min(waiting)
}

deltas <- function(ids) {
  i <- !is.na(ids)
  cbind(ids[i], which(i) - 1) |> split(1:sum(i))
}

superbus <- function(x, y) {
  a <- x[1]; first <- x[2]
  b <- y[1]; delta <- y[2]
  period <- a * b
  set    <- seq(first, period, by = a)
  first  <- set[(set + delta) %% b == 0]
  c(period, first)
}

x <- scan("data/aoc_13", 1L, sep = ",", na = "x")
c(part1 = get_waiting_times(x[-1], x[1]),
  part2 = Reduce(superbus, deltas(x[-1]))[2]
) |> print(15)
