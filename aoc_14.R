parse_instructions <- function(x) {
  x <- gsub("mask = |mem\\[|\\]", "", x) |> strsplit(" = ")
  where_masks <- lengths(x) == 1
  sizes <- diff(c(which(where_masks) - 1, length(x)))

  split(x, rep(seq_along(sizes), sizes)) |>
  lapply(\(x) do.call(rbind.data.frame, x[-1]) |>
    `colnames<-`(c("mem", "vals")) |>
    `attr<-`("mask", strsplit(x[[1]], "")[[1]])
  )
}

# Part one
dec_to_vec36 <- function(n)
  c(rep("0", 4), rev(as.integer(intToBits(n))))

vec_to_dec <- function(v) {
  n <- as.numeric(v)
  pow <- 2 ^ seq(length(n) - 1, 0)
  sum(pow[n == 1])
}

apply_mask <- function(x, old = FALSE) {
  mask <- attr(x, "mask")
  mem  <- if (old) x$mem  else x$vals
  vals <- (if (old) x$vals else x$mem) |>
    lapply(vals, dec_to_vec36)

  j <- mask != if (old) "X" else "0"
  for (i in seq_along(vals))
    vals[[i]][j] <- mask[j]

  if (old) data.frame(mem, sapply(vals, vec_to_dec)) |>
    return()

  new_mem <- lapply(vals, \(x) sapply(float_perms(x), vec_to_dec))
  x <- vector("list", length(new_mem))
  for (i in seq_along(new_mem))
    x[[i]] <- data.frame(new_mem[[i]], mem[i])

  do.call("rbind", x)
}

exec <- function(x, legacy = FALSE) {
  prog <- do.call("rbind", lapply(x, apply_mask, legacy))
  uniqx <- !duplicated(prog[, 1], fromLast = TRUE)
  sum(as.numeric(prog[uniqx, 2]))
}

# Part two
float_perms <- function(v) {
  perm <- expand.grid(rep(list(c("0", "1")), sum(v == "X"))) |>
    sapply(as.character)

  lapply(seq_len(nrow(perm)), \(i) {
    v[v == "X"] <- perm[i, ]; v
  })
}

x <- parse_instructions(readLines("data/aoc_14"))
c(part1 = exec(x, legacy = TRUE),
  part2 = exec(x)) |> print(digits = 14)

# Rewrite attempt
# parse_instructions <- function(x) {
#   x <- gsub("mask = |mem\\[|\\]", "", x) |> strsplit(" = ")
#   is_mask <- lengths(x) == 1
#   sizes <- diff(c(which(is_mask) - 1, length(x)))

#   data.frame(
#     rep(unlist(x[is_mask]), sizes - 1),
#     do.call(rbind, x[!is_mask])
#   ) |> `names<-`(c("mask", "mem", "val"))
# }

# dec_to_bit36 <- function(x)
#   rev(as.integer(intToBits(x))) |>
#     matrix(ncol = length(x)) |>
#     apply(2, \(x) paste(c("0000", x), collapse = "")) |>
#     rev()

# bit36_to_dec <- function(x) {
#   x <- as.numeric(x)
#   pow <- 2 ^ seq(length(x) - 1, 0)
#   sum(pow[x == 1])
# }

# char_to_bool <- function(int)
#   as.logical(as.integer(unlist(strsplit(int, ""))))

# apply_mask <- function(int, mask, legacy = TRUE) {
#   a <- char_to_bool(dec_to_36(int))
#   b <- char_to_bool(mask)
#   i <- if (legacy) !is.na(b) else is.na(b) | b == 1
#   a[i] <- b[i]
#   a <- matrix(a, nrow = 36L)
#   if (legacy) a <- float_perms(a)
#   return(rev(apply(a, 2, bit36_to_dec)))
# }

# float_perms <- function() {
# }

# exec <- function(val, mask, mem, legacy = TRUE) {
#   vals <- apply_mask(val, mask, legacy)
#   written <- !duplicated(mem, fromLast = TRUE)
#   sum(vals[written])
# }

# x <- parse_instructions(readLines("data/aoc_14"))
# c(part1 = with(x, exec(val, mask, mem, TRUE)),
#   part2 = with(x, exec(val, mask, mem, FALSE)))
