input <- paste(readLines("data/aoc_6"), collapse = "\n")
input <- strsplit(input, "\n\n")[[1]]

# Part one
d <- gsub("\n", "", input)
d <- strsplit(d, "")

sum(lengths(sapply(d, unique)))

# Part two
d_group <- strsplit(input, "\n")
d_people_group <- lapply(d_group, strsplit, "")
common <- lapply(d_people_group, function(x) Reduce(intersect, x))

sum(lengths(common))
