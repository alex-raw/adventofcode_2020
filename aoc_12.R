d <- readLines("data/aoc_12")

# Part one
val  <- as.numeric(gsub("[NESWF]", "",  d))
dirx <- as.numeric(gsub("[NESWF].+", "0", chartr("LR", "-+", d)))
dirx <- (cumsum(dirx / 90) %% 4) + 1
dirx <- c("E", "S", "W", "N")[dirx]

instr <- substr(d, 1, 1)
instr[instr == "F"] <- dirx[instr == "F"]
ifelse(instr %in% c("W", "S"), val, -val) |>
  sum(na.rm = TRUE) |> abs()

# Part two
x <- data.frame(instr = substr(d, 1, 1),
                val   = as.numeric(gsub("\\D+", "", d)))

wp <- c(n_s = 1, e_w = 10)
result <- matrix(nrow = nrow(x), ncol = 2)
for (i in seq_len(nrow(x))) {
  instr <- x$instr[i]
  val   <- x$val[i]

  if (instr == "F") result[i, ] <- val * wp else
    wp <- if (instr %in% c("R", "L"))
      switch(as.character(val),
        "180" = -wp,
        "90"  = ifelse(rep(instr == "R", 2),
                  c(-wp[2], wp[1]),
                  c(wp[2], -wp[1])),
        "270" = ifelse(rep(instr == "R", 2),
                  c(wp[2], -wp[1]),
                  c(-wp[2], wp[1])))
    else
      switch(instr,
        N = wp + c(val, 0),
        S = wp - c(val, 0),
        E = wp + c(0, val),
        W = wp - c(0, val),)
}

sum(abs(colSums(result, na.rm = TRUE)))
