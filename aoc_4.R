# {{{ Part one
# workaround for empty line delimiter \n\n
input <- paste(readLines("data/aoc_4"), collapse = "\n")
data <- strsplit(input, "\n\n")[[1]]
data <- gsub("\n", " ", data)

# get rid of cid: since ignored
data <- gsub("cid:\\w+( |$)", "", data)

# split elements per row at " " and count
val <- lengths(strsplit(data, " ")) == 7

sum(val)

# }}}
# {{{ Part two
data2 <- data[val]

# rules
patterns <- c(
  "byr:19[2-9].|200[0-2]( |$)",
  "iyr:201.|2020( |$)",
  "eyr:202.|2030( |$)",
  "hgt:(((1[5-8].|19[0-3])cm)|((59|6[0-9]|7[0-6])in))( |$)", # (150-193cm | 59-76in)
  "hcl:#[a-f0-9]{6}( |$)",
  "ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)",
  "pid:\\d{9}( |$)"
)

for (i in patterns) {
  data2 <- data2[grep(i, data2)]
}

length(data2)
