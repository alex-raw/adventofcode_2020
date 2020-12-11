df_raw <- read.table("data/aoc_2", sep = " ")
range <- stringr::str_split_fixed(df_raw$V1, "-", 2)

df <- data.frame(
  min = as.numeric(range[, 1]),
  max = as.numeric(range[, 2]),
  letter = gsub(":", "", df_raw$V2),
  pass = df_raw$V3
)

# Part one
func <- function(x) {
  lengths(regmatches(x["pass"], gregexpr(x["letter"], x["pass"])))
}

count <- apply(df, 1, func)

poss_pass <- count >= df$min & count <= df$max
sum(poss_pass)

# Part two
min_letter <- substr(df[, "pass"], df[, "min"], df[, "min"])
max_letter <- substr(df[, "pass"], df[, "max"], df[, "max"])

check <- (min_letter == df[, "letter"]) - (max_letter == df[, "letter"])
sum(check != 0)
