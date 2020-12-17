d <- scan("data/aoc_12", "character")

# Part one
instr <- substr(d, 1, 1)
val   <- as.numeric(gsub("\\D+", "", d))
instr2  <- instr[grep("R|L|F", instr)]
val2  <- val[instr %in% c("R", "L", "F")]

dirx_id <- as.numeric(gsub("F.+", "0", chartr("LR", "-+", instr2)))
dirx_id <- (cumsum(dirx_id / 90) %% 4) + 1
instr2 <- c("E", "S", "W", "N")[dirx_id]

move <- function(x) sum(val[instr == x])
move2 <- function(x) sum(val2[instr2 == x], na.rm = TRUE)

a <- move("N") - move("S") + move2("N") - move2("S")
b <- move("E") - move("W") + move2("E") - move2("W")
sum(abs(c(a, b)))

# Part two
x <- data.frame(instr = substr(d, 1, 1),
                val = as.numeric(gsub("\\D+", "", d)))

wp <- c(n_s = 1, e_w = 10)
result <- matrix(nrow = nrow(x), ncol = 2)

for (i in seq_len(nrow(x))) {
  instr <- x$instr[i]
  val   <- x$val[i]

  if (instr == "F") {
    result[i, ] <- val * wp
  } else if (instr %in% c("R", "L")) {
    wp <- switch(as.character(val),
                 "180" = -wp,
                 "90"  = ifelse(rep(instr == "R", 2),
                                yes = c(-wp[2],  wp[1]),
                                no = c(wp[2], -wp[1])),
                 "270" = ifelse(rep(instr == "R", 2),
                                yes = c(wp[2], -wp[1]),
                                no = c(-wp[2],  wp[1]))
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
