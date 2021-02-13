# vector of ingredients
parse_ingredients <- function(input) {
  v <- strsplit(gsub("\\)|,", "", input), " \\(contains ")
  lapply(v, strsplit, " ")
}

get_candidates <- function(allergenes, ingredients) {
  pool <- unique(unlist(allergenes))

  # create boolean matrix for indeces of every allergene occurrence
  m <- matrix(ncol = length(pool),
              nrow = length(allergenes),
              dimnames = list(NULL, pool))

  for (allergene in pool) {
    m[, allergene] <- grepl(allergene, allergenes)
  }

  # column-wise; find ingredients that match all lists with a given allergene
  apply(m, 2, function(y) Reduce(intersect, ingredients[y]))
}

reduce_to_uniq <- function(l) {
  while (any(lengths(l) > 1)) {
    for (i in seq_along(l)) {
      x <- l[[i]]
      if (length(x) == 1) {
        l <- lapply(l, function(v) v[v != x])
        l[[i]] <- x
      }
    }
  }
  unlist(l)
}

solve_1 <- function(l, v) {
  ingredients <- unlist(sapply(l, "[[", 1))
  sum(!ingredients %in% unlist(v))
}

solve_2 <- function(v) paste0(v[order(names(v))], collapse = ",")

d <- parse_ingredients(readLines("data/aoc_21"))
ingredients <- sapply(d, "[[", 1)
allergenes <- sapply(d, "[[", 2)

v <- reduce_to_uniq(get_candidates(allergenes, ingredients))

solve_1(d, v)
cat(solve_2(v), "\n")
