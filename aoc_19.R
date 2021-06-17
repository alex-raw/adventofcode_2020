# Functions to transform rules into regex
group      <- \(x) paste0("(", x, ")")
collapse   <- \(x) unlist(lapply(x, paste, collapse = ""))
extr_regex <- \(x) paste0("^", collapse(x), "$")[1]
msub  <- function(x, patt, repl) {
  for (i in seq_along(patt)) x <- sub(patt[i], repl[i], x)
  x
}

embed_rx <- function(l, forward = TRUE) {
  subrule <- grep("[0-9]", l, invert = forward)
  repl    <- group(collapse(l[subrule]))
  patt    <- paste0("\\b", subrule - 1, "\\b")
  lapply(l, msub, patt, repl)
}

rsub <- function(l, forward = TRUE) {
  old <- vector("list", length(rules))
  repeat {
    old <- l
    l <- embed_rx(l, forward = forward)
    if (identical(l, old)) return(l)
  }
}

solve1 <- function(rules, codes) sum(grepl(extr_regex(rules), codes))
solve2 <- function(rules, codes) {
  x <- old <- 1
  repeat {
    old <- x
    rules <- embed_rx(rules, forward = FALSE)
    x <- lapply(rules, \(x) gsub(" ?\\d+ ?", ".", x)) |> solve1(codes)
    if (x == old) return(x)
  }
}

# remove number before `:` and order to preserve it as index - 1
x <- gsub("\"", "", readLines("data/aoc_19"))
i <- seq_len(grep("^$", x))
codes <- x[-i]
rules <- with(read.table(text = x[i - 1], sep = ":", strip.white = TRUE),
              V2[order(V1)]) |> strsplit(" ")

solve1(rsub(rules), codes)
rules[[8 + 1]]  <- c("42", "|", "42", "8")
rules[[11 + 1]] <- c("42", "31", "|", "42", "11", "31")
solve2(rsub(rules), codes)
