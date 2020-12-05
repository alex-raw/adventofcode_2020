# {{{ Part one
# workaround for empty line delimiter \n\n
input <- paste(readLines("data/aoc_4"), collapse = "\n")
data <- strsplit(input, "\n\n", fixed = TRUE)[[1]]
data <- gsub("\n", " ", data)

# get rid of cid: since ignored
data <- gsub("cid:\\d+($| )", "", data)

# split elements per row at " " and count
val <- lengths(strsplit(data, " ")) == 7

sum(val)

# }}}
# {{{ Part two
data2 <- data[val]

# rules
patterns <- c(
  byr = "byr:19[2-9].|200[0-2]( |$)",
  iyr = "iyr:201.|2020( |$)",
  eyr = "eyr:202.|2030( |$)",
  hgt = "(hgt:(1[5-8].|19[0-3])cm)|(hgt:(59|6[0-9]|7[0-6])in)( |$)",
  hcl = "hcl:#[a-f0-9]{6}( |$)",
  ecl = "ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)",
  pid = "pid:\\d{9}( |$)"
)

for (i in seq_along(patterns)) {
  data2 <- data2[grep(patterns[i], data2)]
  print(data2)
}

length(data2)
