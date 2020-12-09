d <- readLines("data/aoc_7")

y <- d[unlist(sapply("shiny gold", grep, d))]
gsub(" bag.*", "", y)

# " bags."
get_higher <- function(color, d) {
  y <- d[unlist(sapply(color, grep, d))]
  gsub(" bag.*", "", y)
}

# iterate until no new bag colors found
bags <- "shiny gold"
old_bags <- ""

while (!identical(bags, old_bags)) {
  old_bags <- bags
  bags <- unique(get_higher(bags, d))
}

length(bags)

# ---- Part two
extract_numbers <- function(x) {
  y <- as.numeric(unlist(sapply(x, strsplit, "\\D+")))
  y[is.na(y)] <- 0; y
}

# extract rule
get_n_lower <- function(color, d) {
  pattern <- paste(color, ".*contain ")
  rule <- d[unlist(sapply(pattern, grep, d))]
  gsub(".*contain ", "", rule)
}

# main loop
bags <- "shiny gold"
i <- 1
result <- c()
old <- 1

while (!identical(unique(bags), "no other")) {
  # get numbers
  n_lower_bags <- get_n_lower(bags, d)
  n <- lapply(n_lower_bags, extract_numbers)

  # calculate level sum
  vec <- Map("*", old, n)
  result[i] <- sum(unlist(vec))

  # prepare next level
  old <- unlist(vec)
  old <- old[old != 0]

  bags <- gsub(" ?\\d ?|\\.| bags?", "", n_lower_bags)
  bags <- unlist(strsplit(bags, ","))
  i <- i + 1
}

sum(result)

# Test

# bags <- "shiny gold"
# i <- 1
# result <- c()
# old <- 1

#(n_lower_bags <- get_n_lower(bags, d))
#(n <- lapply(n_lower_bags, extract_numbers))
#(vec <- Map("*", old, n))
#(result[i] <- sum(unlist(vec)))
#(old <- unlist(vec))
#(old <- old[old != 0])
)
#(# get lower bag colors)
#(bags <- gsub(" ?\\d ?|\\.| bags?", "", n_lower_bags))
#(bags <- unlist(strsplit(bags, ",")))
#(i <- i + 1)

# result
