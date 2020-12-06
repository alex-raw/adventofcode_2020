input <- paste(readLines("data/aoc_6"), collapse = "\n")
data <- strsplit(input, "\n\n")[[1]]
data <- gsub("\n", ",", data)

# Part one
d <- gsub(",", "", data)
d <- strsplit(d, "")

sum(lengths(sapply(d, unique)))

# Part two
d_group <- strsplit(data, ",")
d_people_group <- lapply(d_group, strsplit, "")
common <- lapply(d_people_group, function(x) Reduce(intersect, x))
sum(lengths(common))
