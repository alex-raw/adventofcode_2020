# ------- Part one
# input character vector with data; output list of vectors with indeces of `#`
parse_tiles <- function(v) {
  ids  <- grepl("Tile", v)
  rows <- gregexpr("#", v[v != "" & !ids])
  list(ids  = as.integer(gsub("\\D", "", v[ids])),
       rows = lapply(rows, as.integer)
  )
}

# reconstruct select columns from row indeces
slice <- Vectorize(vectorize.args = "i",
  function(l, lt, i) {
    m <- matrix(sapply(l, is.element, el = i), nrow = lt)
    apply(m, 2L, which)
})

# extract vertical and horizontal edges as matrices of ragged arrays (lists)
get_vsides <- function(l, lt) slice(l, lt, c(1L, lt))

get_hsides <- function(l, lt) {
  n <- length(l) / lt
  matrix(l[sequence(c(n, n), from = c(1L, lt), by = lt)], ncol = 2L)
}

# for flipped versions
flip_index <- function(v, lt) rev(lt + 1L - v)

# collaps indeces to integer values to get unique identifier
collapse_int <- function(x) as.integer(paste0(x, collapse = ""))

# contruct matrix 1 row per tile, 1 column per side
get_margins <- function(l, use_names = NULL) {
  lt <- do.call(max, l)
  all_sides <- c(h <- get_hsides(l, lt),
                 v <- get_vsides(l, lt),
                 lapply(h, flip_index, lt),
                 lapply(v, flip_index, lt)
  )
  m <- sapply(all_sides, collapse_int)
  sides <- c("top",   "bottom",   "left",   "right",
             "top_f", "bottom_f", "left_f", "right_f")
  matrix(m, ncol = 8, dimnames = list(use_names, sides))
}

# find corners (2 matching sides, -> 4 with flipped) and sides (4 -> 6)
get_side_ids <- function(m) {
  # no match -> count of 1
  n <- table(m)
  m[m %in% names(n[n == 1L])] <- NA_integer_
  n_matches <- rowSums(is.na(m))
  list(corners = rownames(m)[n_matches == 4L],
       sides   = rownames(m)[n_matches == 6L])
}

solve_pt_1 <- function(path) {
  path <- "data/aoc_20_ex"
  d <- parse_tiles(readLines(path))
  m <- get_margins(d$rows, use_names = d$ids)
  sides <- get_side_ids(m)
  prod(as.numeric(sides$corners))
}

system.time(
print(solve_pt_1("data/aoc_20"), digits = 15)
)

# Part one
# turn input into list of logical matrices with id as name
input <- paste0(readLines("data/aoc_20_ex"), collapse = "")
tiles <- chartr("#.", "TF", unlist(strsplit(input, "Tile \\d+:"))[-1])
tiles <- lapply(tiles, function(x) as.logical(unlist(strsplit(x, ""))))
tiles <- lapply(tiles, matrix, ncol = 10, byrow = TRUE)
tile_ids <- as.numeric(unlist(strsplit(gsub("\\W", "", input), "Tile"))[-1])
names(tiles) <- tile_ids

# make a list of borders; 1 = top, 2 = bottom, 3 = left, 4 = right
get_borders <- function(m) cbind(m[1, ], m[nrow(m), ], m[, 1], m[, ncol(m)])
borders <- do.call(cbind, lapply(tiles, get_borders))
borders <- cbind(borders, apply(borders, 2, rev))

# get indeces of matching sides
matches <- rep(NA_integer_, ncol(borders))
for (i in seq(ncol(borders))) {
  v <- which(apply(borders[, i] == borders, 2L, all))
  v <- v[!v == i]
  # use NA not 0 to subset with NA and return NA
  if (length(v) == 0L) next else matches[[i]] <- v
}

matches <- matrix(matches[1:(length(matches) / 2)], nrow = 4)
colnames(matches) <- tile_ids

# Corner tiles miss matches on 2 sides; borders miss 1
no_match <- colSums(is.na(matches))
corners <- names(which(no_match == 2))
sides <- names(which(no_match > 0))
print(prod(as.numeric(corners)), 15)

# --------Part two
# replace match_ids with tile_ids, rep(2) flipped
matches <- apply(matches, 2, function(x) rep(tile_ids, 2)[ceiling(x / 4)])

# start with arbitrary corner and arrange the first side
get_match <- function(x) {
  candidates <- as.character(na.omit(matches[, x]))
  candidates[! candidates %in% ordered_tiles]
}

side_length <- sqrt(length(tiles))
ordered_tiles <- vector("character", length(tiles))

next_match <- corners[1]
for (i in seq_along(tiles)) {
  if (i <= side_length) {
    ordered_tiles[i] <- next_match
    candidates <- get_match(next_match)
    next_match <- candidates[candidates %in% sides][1]
  } else {
    left <- ordered_tiles[i - side_length]
    ordered_tiles[i] <- get_match(left)
  }
}

# rotate
flip_horizontal <- function(m) m[, rev(seq(ncol(m)))]
flip_vertical <- function(m) m[rev(seq(nrow(m))), ]

find_orientation <- function(v1, v2, i) {
  if (i %% side_length == 0) {
    b <- -1; x <- 1
  } else {
    b <- 1; x <- 2
  }

  if (i <= side_length) {
    a <- side_length; y <- 4
  } else {
    a <- -side_length; y <- 3
  }

  h <- which(v1 == v2[i + a])
  v <- which(v1 == v2[i + b])
  c(v, h) == c(x, y)
}

align <- function(m, l) {
  if (identical(l, c(TRUE, TRUE))) {
    m
  } else if (identical(l, c(TRUE, FALSE))) {
    flip_horizontal(m)
  } else if (identical(l, c(FALSE, TRUE))) {
    flip_vertical(m)
  } else {
    flip_horizontal(flip_vertical(m))
  }
}

final_list <- vector("list", length(tiles))
for (i in seq_along(tiles)) {
  next_tile <- tiles[[ordered_tiles[i]]]
  next_matching <- matches[, ordered_tiles[i]]

  # look forward for first row, then look back, 1 top, 2 bottom, 3 left, 4 right
  orientation <- find_orientation(next_matching, ordered_tiles, i)
  final_list[[i]] <- align(next_tile, orientation)
  final_list
}

final_list <- lapply(final_list, function(m) m[-c(1, nrow(m)), -c(1, ncol(m))])
m <- matrix(final_list, ncol = side_length)
lol <- list()
for (i in seq(ncol(m))) {
  lol[[i]] <- do.call(cbind, m[i, ])
  final_grid <- do.call(rbind, lol)
}

# find seamonster
monster_ids <- gregexpr("#",
c("                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
))

list_inc <- function(l, n) lapply(l, function(x) x - n)
scan_pattern <- function(a, b) {
  # dif <- abs(diff(sapply(list(a, b), list_max)))
  dif <- max(sapply(b, max))
  spotted <- logical(dif)

  for (i in seq(0, dif)) {
    spotted[i + 1] <- all(unlist(mapply("%in%", a, list_inc(b, i))))
  }
  return(sum(spotted))
}

rotate <- function(x) t(apply(x, 2, rev))
final_grid2 <- flip_horizontal(rotate(final_grid))
final_grid2 <- final_grid2[, -1]
ifelse(final_grid2, "#", ".")
final_ids <- apply(final_grid2, 1, which)

n <- c()
for (i in seq(0, length(final_ids) - length(monster_ids))) {
  n[i + 1] <- scan_pattern(monster_ids, final_ids[seq_along(monster_ids) + i])
}

# get roughness
monster_volume <- (sum(n) + 1) * sum(sapply(monster_ids, length))
sum(sapply(final_grid, sum)) - monster_volume





# # replace identifiers with index of their matches
# locate_matches <- function(m) {
#   out_m <- m
#   for (i in seq_along(m)) {
#     x <- which(m[i] == m)
#     x <- x[x != i]
#     if (length(x) == 0) out_m[i] <- NA else out_m[i] <- x
#   }
#   out_m
# }

