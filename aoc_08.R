find_targets <- function(x) within(x, {
  jmp_val <- ifelse(instr == "jmp", val, 1)
  target  <- seq_along(jmp_val) + jmp_val
})

row_path <- function(x, iter = 1e4) {
  nxt <- hits <- 1
  for (i in 2:iter) {
    nxt <- hits[i] <- x[nxt]
    if (anyDuplicated(hits)) return(unique(hits))
  }
}

# get acc value from every column that is hit before loop
sum_acc <- function(x)
  with(x[row_path(x$target), ],
       sum(val[instr == "acc"], na.rm = TRUE))

# find rows of which any must lead to the last row
find_exit <- function(x, iter =	1e4) {
  nxt <- length(x)
  goal <- vector("list", iter)
  for (i in 1:iter) {
    goal[[i]] <- nxt <- which(x %in% nxt)
    if (!length(nxt)) return(unlist(goal))
  }
}

# check if any row above was hit already in case a jmp is wrong
find_dups <- function(goal, path) goal[(goal - 1) %in% path] - 1

# switch corrupt; recalculate jumps. doesn't work if nop is wrong
fix <- function(x) within(x, {
  candidate <- find_dups(find_exit(target), row_path(target))
  jmp_val[candidate] <- 1
  target <- seq_along(jmp_val) + jmp_val
})

d <- read.table("data/aoc_08", col.names = c("instr", "val")) |>
  find_targets()

c(part1 = sum_acc(d),
  part2 = sum_acc(fix(d)))
