# workaround for empty line delimiter with 2 spaces
parseports <- function(x)
  paste(x, collapse = " ") |> strsplit("  ") |> unlist()

count_fields <- function(x)
  lengths(gregexpr("(?<!cid):", x, perl = TRUE))

count_valid <- function(x) {
  rules <- c(
    "byr:19[2-9].|200[0-2]",
    "iyr:201.|2020",
    "eyr:202.|2030",
    "hgt:((1[5-8].|19[0-3])cm|((59|6[0-9]|7[0-6])in))",
    "hcl:#[a-f0-9]{6}",
    "ecl:(amb|blu|brn|gry|grn|hzl|oth)",
    "pid:\\d{9}"
  ) |> paste0("( |$)")

  for (i in rules) x <- x[grepl(i, x)]
  length(x)
}

d <- parseports(readLines("data/aoc_04"))
c(part1 = sum(count_fields(d) == 7),
  part2 = count_valid(d))
