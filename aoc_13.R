d <- readLines("data/aoc_13")

timestmp <- as.numeric(d[1])
bus_id_raw <- as.numeric(strsplit(d[[2]], ",")[[1]])

# Part one
bus_id <- bus_id_raw[!is.na(bus_id_raw)]

schedule <- mapply(seq, 0, bus_id + timestmp, by = bus_id)
waiting  <- sapply(schedule, function(x) {
                  dif <- x - timestmp
                  min(dif[dif > 0])
})

bus_id[which(waiting == min(waiting))] * min(waiting)

# {{{ Part two
deltas <- which(!is.na(bus_id_raw)) - 1
vals <- cbind(bus_id, deltas)
vals <- split(vals, seq(nrow(vals)))

superbus <- function(x, y) {
  a <- x[1]; first <- x[2]
  b <- y[1]; delta <- y[2]

  period <- a * b
  set   <- seq(first, period, by = a)
  first  <- set[which((set + delta) %% b == 0)]

  return(c(period, first))
}

result <- Reduce(superbus, vals)[2]
print(result, digits = 15)
