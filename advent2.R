library(stringr)

df_raw <- read.table("day2_data", sep = " ")
range <- str_split_fixed(df_raw$V1, "-", 2)

df <- data.frame(
  min = as.numeric(range[, 1]),
  max = as.numeric(range[, 2]),
  letter = gsub(":", "", df_raw$V2),
  pass = df_raw$V3
)

func <- function(x) {
  lengths(regmatches(df[x, "pass"], gregexpr(df[x, "letter"], df[x, "pass"])))
}

vec <- c()
for (i in seq_len(nrow(df))) {
  vec[i] <- func(i)
}

poss_pass <- vec >= df$min & vec <= df$max
sum(poss_pass)
