input <- gsub("\"", "", readLines("data/aoc_19"))
blank <- grep("^$", input)

codes <- input[seq(blank + 1, length(input))]
rules <- input[seq(1, blank - 1)]

# Part one
# remove number before `:` and order to preserve it as index - 1
rules <- strsplit(rules, ": ")
index <- sapply(rules, "[", 1)
rules <- sapply(rules, "[", 2)[order(as.numeric(index))]
rules <- lapply(rules, function(x) unlist(strsplit(x, " ")))

# Functions to transform rules into regex
collapse   <- function(x) unlist(lapply(x, paste, collapse = ""))
make_group <- function(x) paste0("(", x, ")")
extr_regex <- function(x) paste0("^", collapse(x), "$")
multi_sub  <- function(x, patt, repl) {
  for (i in seq(length(patt))) x <- sub(patt[i], repl[i], x)
  x
}

substitute_patterns <- function(l, forward = TRUE) {
    subrule <- grep("[0-9]", l, invert = forward)
    repl    <- make_group(collapse(l[subrule]))
    patt    <- paste0("\\b", subrule - 1, "\\b")
    lapply(l, multi_sub, patt, repl)
}

iterate_substitute <- function(l, forward = TRUE) {
  old <- vector(length(rules), mode = "list")
  while(!identical(l, old)) {
    old <- l
    l <- substitute_patterns(l, forward = forward)
  }
  l
}

regex <- extr_regex(iterate_substitute(rules))[1]
result_1 <- length(grep(regex, codes))

# Part Two
rules[[8 + 1]]  <- c("42", "|", "42", "8")
rules[[11 + 1]] <- c("42", "31", "|", "42", "11", "31")

regex_list <- iterate_substitute(rules)

result_2 <- 1; former_result <- 0
while(result_2 != former_result) {
  former_result <- result_2
  regex_list <- substitute_patterns(regex_list, forward = FALSE)
  regex <- lapply(regex_list, gsub, pattern = " ?\\d+ ?", replacement = ".")
  regex <- extr_regex(regex)[1]
  result_2 <- length(grep(regex, codes))
}

result_1
result_2
