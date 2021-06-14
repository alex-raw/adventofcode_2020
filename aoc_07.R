pick  <- \(li, id) lapply(li, "[", id)
named_int  <- \(x) as.integer(x[c(T, F)]) |> `names<-`(x[c(F, T)])
build_tree <- \(x) pick(x, -1L) |> lapply(named_int) |> `names<-`(pick(x, 1L))

parse_bags <- function(x)
    gsub(" contain| bags?|,|\\.", "", x) |>
    strsplit("(?<=\\d) | (?=\\d)", perl = TRUE) |>
    build_tree()

in_names    <- \(x, y) vapply(x, \(x) any(names(x) %in% y), logical(1L))
find_parent <- \(x, y) names(x[in_names(x, y)])

gather_parents <- function(x, node, iter = 100L) {
    ans <- vector("list", iter)
    for (i in 1:iter) {
        ans[[i]] <- node <- find_parent(x, node)
        if (!length(node)) break
    }
    length(unique(unlist(ans)))
}

count_children <- function(x, init) {
    node <- n <- 1L
    names(node) <- init
    while (length(node)) {
        node <- Map("*", node, x[names(node)]) |> unname() |> unlist()
        n <- n + sum(node)
    }
    n - 1L
}

x <- parse_bags(readLines("data/aoc_07"))
c(part1 = gather_parents(x, "shiny gold"),
    part2 = count_children(x, "shiny gold"))
