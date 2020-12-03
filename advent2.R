library(stringr)

df_raw <- read.table("day2_data", sep = " ")
range <- str_split_fixed(df_raw$V1, "-", 2)

df <- data.frame(
  min = as.numeric(range[,1]),
  max = as.numeric(range[,2]),
  letter = gsub(":", "", df_raw$V2),
  pass = df_raw$V3
)

lengths(regmatches(df[, "pass"], gregexpr(df[, "letter"], df[, "pass"])))
