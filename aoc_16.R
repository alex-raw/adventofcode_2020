input <- readLines("data/aoc_16")
blank <- grep("^$", input) - 1

scan_ <- function(x) scan(text = x, sep = ",", quiet = TRUE)

mine <- scan_(input[blank[2]])
nearby <- input[-seq_len(blank[2] + 2)]
nearby <- matrix(scan_(nearby), length(nearby))

header <- input[1:blank[1]] |>
  strsplit(": | or |-") |>
  do.call(what = rbind.data.frame) |>
  `names<-`(c("var", "a1", "b1", "a2", "b2")) |>
  type.convert()

in_range <- with(header,
  sapply(nearby, \(x) any((x >= a1 & x <= b1) | (x >= a2 & x <= b2)))
)

sum(nearby[!in_range])

# Remove invalid
valid <- rowSums(matrix(in_range, nrow(nearby))) == ncol(nearby) # TODO:
nearby[lol, ]

valid_tick <- nearby[sapply(nearby, \(x) all(x %in% valid))]
m <- do.call(rbind, valid_tick)

# Determine fields which match the possible number ranges
which_field <- function(v, ranges) {
  match <- sapply(ranges, \(x) all(v %in% x))
  return(names(which(match)))
}

matches <- apply(m, 2, which_field, ranges)
fields <- integer(length(ranges))
names(fields) <- names(ranges)

# Assign unique field to column index and get rid of this field; repeat
for (i in seq_along(matches)) {
  ids <- lengths(matches) == 1
  name <- unlist(matches[ids])
  fields[name] <- which(ids)

  # delete field
  matches <- lapply(matches, function(x) x[!x == name])
}

departure <- fields[grepl("departure", names(fields))]
prod(mine[departure])
