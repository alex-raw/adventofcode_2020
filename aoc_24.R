subtile <- function(x, y) gsub(x = x,
    paste0("(?<![sn])", y),
    paste0("_", y),
    perl = TRUE
)

split_tiles <- function(x)
  subtile(x, "e") |> subtile("w") |>
  strsplit("(?<=..)", perl = TRUE)

elim <- function(x) mapply(
  \(a, b) x[a] - x[b],
  c("_e", "ne", "se"),
  c("_w", "sw", "nw")
)

distances <- readLines("data/aoc_24_ex") |>
  split_tiles() |>
  lapply(table) |>
  lapply(elim)# |>
  duplicated()

# hmm that's not how a hex field works
