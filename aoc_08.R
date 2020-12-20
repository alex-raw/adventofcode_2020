d <- read.table("data/aoc_08", col.names = c("instr", "val"))

# Part one
d$jmp_val <- d$val
d[d$instr != "jmp", "jmp_val"] <- 1
d$target <- seq(nrow(d)) + d$jmp_val

row_path <- function(x) {
  nxt <- hits <- i <- 1
  while (!any(duplicated(hits))) {
    nxt <- hits[i + 1] <- x[nxt, "target"]
    i <- i + 1
  }
  return(unique(hits))
}

# get acc value from every column that is hit before loop
sum_acc <- function(p) {
  d <- d[p, ]
  sum(d[d$instr == "acc", "val"], na.rm = TRUE)
}

p <- row_path(d)
sum_acc(p)

#{{{ Part two
# find rows of which any must lead to the last row
nxt <- nrow(d)
goal <- list()
i <- 1

while (length(nxt) > 0) {
  which(d$target %in% nxt) -> nxt -> goal[[i]]
  i + 1 -> i
}

# check if any row above was hit already in case a jmp is wrong
goal <- unlist(goal)
candidate <- goal[(goal - 1) %in% p] - 1

# switch corrupt; recalculate jumps. doesn't work if nop is wrong
d[candidate, "jmp_val"] <- 1
d$target <- 1:nrow(d) + d$jmp_val

sum_acc(row_path(d))
