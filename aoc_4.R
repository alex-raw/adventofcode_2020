# {{{ Part one
# workaround for empty line delimiter \n\n
input <- paste(readLines("data/aoc_4"), collapse = ";")
d <- strsplit(input, ";;")[[1]]
d <- gsub(";", " ", d)
d <- gsub("cid:\\w+( |$)", "", d)

# split elements per row at " " and count
val <- lengths(strsplit(d, " ")) == 7
sum(val)

# {{{ Part two
# rules
patterns <- c(
  "byr:19[2-9].|200[0-2]( |$)",
  "iyr:201.|2020( |$)",
  "eyr:202.|2030( |$)",
  "hgt:(((1[5-8].|19[0-3])cm)|((59|6[0-9]|7[0-6])in))( |$)",
  "hcl:#[a-f0-9]{6}( |$)",
  "ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)",
  "pid:\\d{9}( |$)"
)

d2 <- d[val]

for (i in patterns) {
  d2 <- d2[grep(i, d2)]
}

length(d2)

##{{{ Alternative Part one
#d <- paste(readLines("data/aoc_4"), collapse = ";")
#d <- strsplit(d, ";;")[[1]]
#d <- gsub("cid:", "", d)
#d <- gsub("[^:]", "", d)
#sum(nchar(d) == 7)
##}}}
