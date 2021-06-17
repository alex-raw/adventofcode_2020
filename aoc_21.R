# vector of ingredients
parse_ingredients <- function(input)
  strsplit(gsub("\\)|,", "", input), " \\(contains ") |> lapply(strsplit, " ")

get_candidates <- function(allergenes, ingredients) {
  # create boolean matrix for indeces of every allergene occurrence
  pool <- unique(unlist(allergenes))
  m <- matrix(F, length(allergenes), length(pool), dimnames = list(NULL, pool))

  for (allergene in pool)
    m[, allergene] <- grepl(allergene, allergenes)

  # column-wise; find ingredients that match all lists with a given allergene
  apply(m, 2, \(y) Reduce(intersect, ingredients[y]))
}

# check if string is the only candidate, if yes, remove from all other lists
# until every list has only one element
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

d <- parse_ingredients(readLines("data/aoc_21"))
ingredients <- sapply(d, "[[", 1)
allergenes <- sapply(d, "[[", 2)

result <- reduce_to_uniq(get_candidates(allergenes, ingredients))
result <- result[order(names(result))]

c(part1 = sum(!unlist(ingredients) %in% result),
  part2 = paste0(result, collapse = ","))
