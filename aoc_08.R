d <- read.table("data/aoc_08", col.names = c("instr", "val"))

# Part one
d$jmp_val <- d$val
d[d$instr == "acc" | d$instr == "nop", "jmp_val"] <- 1
d$target <- 1:nrow(d) + d$jmp_val

row_path <- function() {
  nxt <- hits <- i <- 1
  while (!any(duplicated(hits))) {
    nxt <- hits[i + 1] <- d[nxt, "target"]
    i <- i + 1
  }
  return(unique(hits))
}

# get acc value from every column that is hit before loop
sum_acc <- function(p) {
  sum(d[p, ][d[p, ]$instr == "acc", "val"], na.rm = TRUE)
}

p <- row_path()
sum_acc(p)

# Part two
# find rows of which any must lead to the last row
nxt <- nrow(d)
goal <- list()
i <- 1

while (length(nxt) > 0) {
  goal[[i]] <- nxt <- which(d$target %in% nxt)
  i <- i + 1
}

# check if any row above was hit already in case a jmp is wrong
goal <- unlist(goal)
candidate <- goal[(goal - 1) %in% p] - 1

if (d[candidate, "instr"] == "jmp") {
  # switch corrupt; recalculate jumps
  d[candidate, "jmp_val"] <- 1
  d$target <- 1:nrow(d) + d$jmp_val

  sum_acc(row_path())

} else {
  print("Nope. Not prepared for nop!")
}
