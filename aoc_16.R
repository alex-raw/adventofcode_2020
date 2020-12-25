input       <- readLines("data/aoc_16")
line_breaks <- grep("^$", input) - 1

# Part one
parse_num   <- function(x) lapply(strsplit(x, ","), as.numeric)
my_tick     <- parse_num(input[line_breaks[2]])[[1]]
nearby_tick <- parse_num(input[-seq(line_breaks[2] + 2)])

header    <- input[1:line_breaks[1]]
header    <- strsplit(header, ": ")
header    <- as.data.frame(do.call(rbind, header))

header[2] <- sapply(header[2], function(x) {
               # prepare to evaluate ranges in the form c(a:b, c:d)
               x <- gsub(" or ", ",",
                    gsub("-",    ":", x))
               x <- paste0("c(", x , ")")
})

ranges <- apply(header[2], 1, function(x) eval(parse(text = x)))
names(ranges) <- unlist(header[1])

# Which numbers in `nearby_tick` are not in the `valid` ranges from the header
valid <- unique(unlist(ranges))
n <- unlist(nearby_tick)

sum(n[which(!n %in% valid)])

# {{{ Part two
# Remove invalid
valid_tick <- nearby_tick[sapply(nearby_tick, function(x) all(x %in% valid))]
m <- do.call(rbind, valid_tick)

# Determine fields which match the possible number ranges
which_field <- function(v, ranges) {
  match <- sapply(ranges, function(x) all(v %in% x))
  return(names(which(match)))
}

matches <- apply(m, 2, which_field, ranges)
fields <- integer(length(ranges))
names(fields) <- names(ranges)

# Assign unique field to column index and get rid of this field; repeat
for (i in seq(length(matches))) {
  match_index <- which(lengths(matches) == 1)
  name <- unlist(matches[match_index])
  fields[name] <- match_index

  # delete field
  matches <- lapply(matches, function(x) x[-which(x == name)])
}

departure <- fields[grep("departure", names(fields))]
prod(my_tick[departure])
