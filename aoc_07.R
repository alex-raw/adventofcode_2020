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

# -------

# Pick out the parents and children:
parents <- str_split_fixed(bags_list, pattern = " bags contain ", n = 2)[, 1]
children <- str_split_fixed(bags_list, pattern = " bags contain ", n = 2)[, 2]

# make the children into lists of names and values
split_children <- function(x) {

  if (str_detect(x[1], "no other")) {
    list(colors = NULL, n = NULL)
  } else{
    list(colors = str_remove_all(x, "[0-9]+ | bags?|\\."),
         n = as.numeric(str_extract(x, "[0-9]+"))
    )
  }
}

parent_child <- children %>%
  lapply(str_split, pattern = ", ", simplify = TRUE) %>%
  lapply(split_children)
names(parent_child) <- parents

parent_child[1]
bags_list[1]

# So now I want to write a function that can take in a color and find the
# count of children below it.
count_children <- function(color, n) {

  # Whatever color we called it on, get their children and counts:
  new_children <- parent_child[[color]]$colors
  new_n <- parent_child[[color]]$n

  # If we're at the leaf of the tree:
  if (length(new_children) < 1) {
    return(n)
  } else {  # otherwise, call the function
    child_count <- map2_dbl(
      .x = new_children,
      .y = new_n,
      .f = count_children
    )
    n + n * sum(child_count)
  }
}

# this should return 1 more than the total since the final
# line will count the gold bag itself:
count_children("shiny gold", 1)
