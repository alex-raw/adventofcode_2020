d <- readLines("data/aoc_12")

# Part one
x <- data.frame(instr = substr(d, 1, 1),
                val   = as.numeric(gsub("[NESWF]", "",  d)),
                dirx  = as.numeric(gsub("[NESWF].+", "0", chartr("LR", "-+", d))))

x$dirx <- (cumsum(x$dirx / 90) %% 4) + 1
x$dirx <- c("E", "S", "W", "N")[x$dirx]

x[x$instr == "F", "instr"] <- x[x$instr == "F", "dirx"]
x[x$instr %in% c("W", "S"), "val"] <- -x[x$instr %in% c("W", "S"), "val"]

abs(sum(x$val, na.rm = TRUE))


# {{{ Part two
x <- data.frame(instr = substr(d, 1, 1),
                val   = as.numeric(gsub("\\D+", "", d)))

wp <- c(n_s = 1, e_w = 10)
result <- matrix(nrow = nrow(x), ncol = 2)

for (i in seq_len(nrow(x))) {
  instr <- x[i, "instr"]
  val   <- x[i, "val"]

  if (instr == "F") {
    result[i, ] <- val * wp
  } else if (instr %in% c("R", "L")) {
    wp <- switch(as.character(val),
                 "180" = -wp,
                 "90"  = ifelse(rep(instr == "R", 2),
                                yes = c(-wp[2],  wp[1]),
                                no  = c( wp[2], -wp[1])),
                 "270" = ifelse(rep(instr == "R", 2),
                                yes = c( wp[2], -wp[1]),
                                no  = c(-wp[2],  wp[1])),
    )
  } else {
    wp <- switch(instr,
                 N = wp + c(val, 0),
                 S = wp - c(val, 0),
                 E = wp + c(0, val),
                 W = wp - c(0, val),
    )
  }
}

sum(abs(colSums(result, na.rm = TRUE)))
