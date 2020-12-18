d <- readLines("data/aoc_07")

start_time <- Sys.time()

get_higher <- function(color, d) {
  y <- d[unlist(sapply(color, grep, d))]
  gsub(" bag.*", "", y)
}

#{{{ iterate until no new bag colors found
bags <- "shiny gold"
old_bags <- ""

while (!identical(bags, old_bags)) {
  old_bags <- bags
  bags <- unique(get_higher(bags, d))
}

length(bags)

#{{{ ---- Part two
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
old <- i <- 1
result <- c()

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

end_time <- Sys.time()

end_time - start_time
