# input character vector with data; output list of vectors with indeces of `#`
parse_tiles <- function(v) {
  ids  <- grepl("Tile", v)
  rows <- gregexpr("#", v[v != "" & !ids])
  list(ids  = as.integer(gsub("\\D", "", v[ids])),
       rows = lapply(rows, as.integer))
}

# reconstruct select columns from row indeces
slice <- (function(l, lt, i)
  sapply(l, "%in%", x = i) |> matrix(lt) |> apply(2L, which)
  ) |> Vectorize(vectorize.args = "i")

# extract vertical and horizontal edges as matrices of ragged arrays (lists)
get_vsides <- function(l, lt) slice(l, lt, c(1L, lt))

get_hsides <- function(l, lt) {
  n <- length(l) / lt
  matrix(l[sequence(c(n, n), c(1L, lt), lt)], ncol = 2L)
}

flip_index <- function(v, lt) rev(lt + 1L - v)

# collaps indeces to integer values to get unique identifier
collapse_int <- function(x) as.integer(paste0(x, collapse = ""))

# contruct matrix 1 row per tile, 1 column per side
get_margins <- function(l, use_names = NULL) {
  lt <- do.call(max, l)
  all_sides <- c(h <- get_hsides(l, lt),
                 v <- get_vsides(l, lt),
                 lapply(h, flip_index, lt),
                 lapply(v, flip_index, lt))
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
  d <- parse_tiles(readLines("data/aoc_20_ex"))
  m <- get_margins(d$rows, use_names = d$ids)
  sides <- get_side_ids(m)
  prod(as.numeric(sides$corners))
}

print(solve_pt_1("data/aoc_20"), digits = 15)
