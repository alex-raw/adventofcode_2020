d <- paste(readLines("data/aoc_14"), collapse = ",")
# d <- paste(readLines("data/aoc_14_ex"), collapse = ",")
d <- gsub("mem\\[|\\]", "", d)
d <- strsplit(strsplit(d, "mask = ")[[1]], ",")[-1]   # "mask = " first list empty
d <- lapply(d, strsplit, " = ")
d <- lapply(d, function(l) {
  mask <- strsplit(l[[1]], "")[[1]]
  mem  <- unlist(tail(l, -1))[c(TRUE, FALSE)]
  vals <- unlist(tail(l, -1))[c(FALSE, TRUE)]
  list(mask, mem, vals)
})

# Part one
dec_to_vec36 <- function(n) {
  n <- rev(as.integer(intToBits(n)))
  c(rep("0", 4), n)
}

vec_to_dec <- function(v) {
  n <- as.numeric(v)
  pow <- 2 ^ ((length(n) - 1):0)
  sum(pow[n == 1])
}

apply_mask <- function(l) {
  mask <- l[[1]]
  mem  <- l[[2]]
  vals <- lapply(l[[3]], dec_to_vec36)

  for (i in seq_along(vals)) {
    vals[[i]][mask != "X"] <- mask[mask != "X"]
  }

  new_vals <- sapply(vals, vec_to_dec)
  data.frame(mem, new_vals)
}

exec <- function(x, legacy = FALSE) {
  if (legacy) {
    mask_fun <- apply_mask
  } else {
    mask_fun <- apply_new_mask
  }
  prog <- do.call("rbind", lapply(x, mask_fun))
  uniqx <- !duplicated(prog[, 1], fromLast = TRUE)
  return(sum(as.numeric(prog[uniqx, 2])))
}

print(exec(d, legacy = TRUE), digits = 14)

# Part two
float_perms <- function(v) {
  perm <- expand.grid(rep(list(c("0", "1")), sum(v == "X")))
  perm <- sapply(perm, as.character)   # why stringAsFactor = F ignored?

  lapply(seq_len(nrow(perm)), function(i) {
    v[v == "X"] <- perm[i, ]
    return(v)
  })
}

apply_new_mask <- function(l) {
  mask <- l[[1]]
  mem  <- lapply(l[[2]], dec_to_vec36)
  vals <- l[[3]]

  for (i in seq_along(vals)) {
    mem[[i]][mask != "0"] <- mask[mask != "0"]
  }

  new_mem <- lapply(mem, float_perms)
  new_mem <- lapply(new_mem, function(x) unlist(lapply(x, vec_to_dec)))

  x <- vector("list", length(new_mem))
  for (i in seq_along(new_mem)) {
    x[[i]] <- data.frame(new_mem[[i]], vals[i])
  }
  colnames(x) <- NULL

  return(do.call("rbind", x))
}

print(exec(d), digits = 13)
