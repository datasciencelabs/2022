## ---- -------------------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(rvest)


## ----------------------------------------------------------------------------------------------
data(reported_heights)


## ----------------------------------------------------------------------------------------------
class(reported_heights$height)


## ----------------------------------------------------------------------------------------------
x <- as.numeric(reported_heights$height)


## ----------------------------------------------------------------------------------------------
head(x)


## ----------------------------------------------------------------------------------------------
sum(is.na(x))


reported_heights |> 
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height)) |> 
  head(n=10)

## TESTING AND IMPROVING

## ----------------------------------------------------------------------------------------------
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}


## ----------------------------------------------------------------------------------------------
problems <- reported_heights |> 
  filter(not_inches(height)) |>
  pull(height)
length(problems)


## ---- 
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) |> head(n=10) |> cat()


## ---- 
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) |> head(n=10) |> cat()


## ---- -------------------------------------------------------------------------------
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] |> head(n=10) |> cat()


## ----------------------------------------------------------------------------------------------
s <- "Hello!"


## ----------------------------------------------------------------------------------------------
s <- 'Hello!'


## ---- eval=FALSE-------------------------------------------------------------------------------
## s <- `Hello`


## ---- eval=FALSE-------------------------------------------------------------------------------
## s <- "10""


## ----------------------------------------------------------------------------------------------
s <- '10"'


## ----------------------------------------------------------------------------------------------
s


## ----------------------------------------------------------------------------------------------
s <- "10\""


## ----------------------------------------------------------------------------------------------
cat(s)


## ----------------------------------------------------------------------------------------------
s <- "5'"
cat(s)


## ---- eval=FALSE-------------------------------------------------------------------------------
## s <- '5'10"'


## ---- eval=FALSE-------------------------------------------------------------------------------
## s <- "5'10""


## ----------------------------------------------------------------------------------------------
s <- '5\'10"'
cat(s)


## ----------------------------------------------------------------------------------------------
s <- "5'10\""
cat(s)


## ---- eval=FALSE-------------------------------------------------------------------------------
## pattern <- ","
## str_detect(murders_raw$total, pattern)


## ----------------------------------------------------------------------------------------------
str_subset(reported_heights$height, "cm")


## ----------------------------------------------------------------------------------------------
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)


## ----------------------------------------------------------------------------------------------
str_detect(s, "cm") | str_detect(s, "inches")


## ----------------------------------------------------------------------------------------------
str_detect(s, "cm|inches")


## ----------------------------------------------------------------------------------------------
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)


str_view(s, pattern)


str_view_all(s, pattern)


str_view(s, "[56]")


## ----------------------------------------------------------------------------------------------
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")


pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view_all(s, pattern)


pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)




## ----------------------------------------------------------------------------------------------
pattern <- "^[4-7]'\\d{1,2}\"$"


## ----------------------------------------------------------------------------------------------
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)


## ----------------------------------------------------------------------------------------------
identical("Hi", "Hi ")


## ----------------------------------------------------------------------------------------------
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)


## ----------------------------------------------------------------------------------------------
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")


## ----------------------------------------------------------------------------------------------
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))


## ----------------------------------------------------------------------------------------------
pattern <- "[^a-zA-Z]\\d"
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "B2", "C0", "E4")
str_detect(yes, pattern)
str_detect(no, pattern)


## ----------------------------------------------------------------------------------------------
pattern <- "(?=\\w{8,16})(?=^[a-z|A-Z].*)(?=.*\\d+.*).*"
yes <- c("Ihatepasswords1", "password1234")
no <- c("sh0rt", "Ihaterpasswords", "7X%9,N`yrYG92b7")
str_detect(yes, pattern)
str_detect(no, pattern)
str_extract(yes, pattern)


## ----------------------------------------------------------------------------------------------
pattern_without_groups <- "^[4-7],\\d*$"


## ----------------------------------------------------------------------------------------------
pattern_with_groups <-  "^([4-7]),(\\d*)$"


## ----------------------------------------------------------------------------------------------
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)


## ----------------------------------------------------------------------------------------------
str_match(s, pattern_with_groups)


## ----------------------------------------------------------------------------------------------
str_extract(s, pattern_with_groups)


## ----------------------------------------------------------------------------------------------
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))


problems[c(2, 10, 11, 12, 15)] |> str_view(pattern)


## ----------------------------------------------------------------------------------------------
str_subset(problems, "inches")


## ----------------------------------------------------------------------------------------------
str_subset(problems, "''")


## ----------------------------------------------------------------------------------------------
pattern <- "^[4-7]'\\d{1,2}$"


## ----------------------------------------------------------------------------------------------
problems |> 
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |> 
  sum()


## ----------------------------------------------------------------------------------------------
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems |> 
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |> 
  sum()


## ----------------------------------------------------------------------------------------------
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")


## ----------------------------------------------------------------------------------------------
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"


## ----------------------------------------------------------------------------------------------
str_subset(problems, pattern_with_groups) |> head()


## ----------------------------------------------------------------------------------------------
str_subset(problems, pattern_with_groups) |> 
  str_replace(pattern_with_groups, "\\1'\\2") |> head()


## ----------------------------------------------------------------------------------------------
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & 
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights |> 
  filter(not_inches_or_cm(height)) |>
  pull(height)
length(problems)


## ----------------------------------------------------------------------------------------------
converted <- problems |> 
  str_replace("feet|foot|ft", "'") |> # convert feet symbols to '
  str_replace("inches|in|''|\"", "") |>  # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")# change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)


## ----------------------------------------------------------------------------------------------
converted[!index]


## ----------------------------------------------------------------------------------------------
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")


## ----------------------------------------------------------------------------------------------
str_replace(s, "^([56])'?$", "\\1'0")


## ----------------------------------------------------------------------------------------------
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"


## ----------------------------------------------------------------------------------------------
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")


## ----------------------------------------------------------------------------------------------
s <- "Hi "
cat(s)
identical(s, "Hi")


## ----------------------------------------------------------------------------------------------
str_trim("5 ' 9 ")


## ----------------------------------------------------------------------------------------------
s <- c("Five feet eight inches")
str_to_lower(s)


## Final answer
## ----------------------------------------------------------------------------------------------
convert_format <- function(s){
  s |>
    str_replace("feet|foot|ft", "'") |> 
    str_replace_all("inches|in|''|\"|cm|and", "") |>  
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") |> 
    str_replace("^([56])'?$", "\\1'0") |> 
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") |>  
    str_trim() 
}

library(english)
words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, words(i), as.character(i))
  s
}


## ----------------------------------------------------------------------------------------------
converted <- problems |> words_to_numbers() |> convert_format()
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


## ----------------------------------------------------------------------------------------------
s <- c("5'10", "6'1")
tab <- data.frame(x = s)


## ----------------------------------------------------------------------------------------------
tab |> separate(x, c("feet", "inches"), sep = "'")


## ----------------------------------------------------------------------------------------------
library(tidyr)
tab |> extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


## ----------------------------------------------------------------------------------------------
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)


## ----------------------------------------------------------------------------------------------
tab |> separate(x, c("feet","inches"), sep = "'", fill = "right")


## ----------------------------------------------------------------------------------------------
tab |> extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights |> 
  mutate(original = height, 
         height = words_to_numbers(height) |> convert_format()) |>
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) |> 
  mutate_at(c("height", "feet", "inches"), as.numeric) |>
  mutate(guess = 12 * feet + inches) |>
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height,  #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA))) |>
  mutate(height = ifelse(is.na(height) & 
                           inches < 12 & between(guess, smallest, tallest),
                         guess, height)) |>
  select(-guess)

new_heights |>
  filter(not_inches(original)) |>
  select(original, height) |>
  arrange(height) |>
  View()



##############################
### String splitting
##############################


## ----------------------------------------------------------------------------------------------
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)


## ----------------------------------------------------------------------------------------------
lines |> head()


## ----------------------------------------------------------------------------------------------
x <- str_split(lines, ",") 
x |> head(2)


## ----------------------------------------------------------------------------------------------
col_names <- x[[1]]
x <- x[-1]


map(x, 1)


## ----------------------------------------------------------------------------------------------
dat <- tibble(map_chr(x, 1),  
              map_chr(x, 2),
              map_chr(x, 3),
              map_chr(x, 4),
              map_chr(x, 5)) |>
  mutate_all(parse_guess) |>
  setNames(col_names)
dat |> head()


## ----------------------------------------------------------------------------------------------
dat <- x |>
  transpose() |>
  map( ~ parse_guess(unlist(.))) |>
  setNames(col_names) |> 
  as_tibble() 


## ----------------------------------------------------------------------------------------------
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
colnames(x) <- col_names
x |> as_tibble() |>
  mutate_all(parse_guess) |>
  head(5)

########################
### Table from PDF
########################


## ----------------------------------------------------------------------------------------------
library(dslabs)
data("research_funding_rates")
research_funding_rates |> 
  select("discipline", "success_rates_men", "success_rates_women")




## library("pdftools")
## temp_file <- tempfile()
## url <- paste0("https://www.pnas.org/content/suppl/2015/09/16/",
##               "1510159112.DCSupplemental/pnas.201510159SI.pdf")
## download.file(url, temp_file)
## txt <- pdf_text(temp_file)
## file.remove(temp_file)
## raw_data_research_funding_rates <- txt[2]


## ----------------------------------------------------------------------------------------------
data("raw_data_research_funding_rates")


## ----------------------------------------------------------------------------------------------
tab <- str_split(raw_data_research_funding_rates, "\n")


## ----------------------------------------------------------------------------------------------
tab <- tab[[1]]


## ----------------------------------------------------------------------------------------------
the_names_1 <- tab[3]
the_names_2 <- tab[4]


## ---- -------------------------------------------------------------------------------
cat(substr(the_names_1, 1, options()$width))
cat(substr(the_names_1, options()$width, nchar(the_names_1)))


## ----------------------------------------------------------------------------------------------
the_names_1 <- the_names_1 |>
  str_trim() |>
  str_replace_all(",\\s.", "") |>
  str_split("\\s{2,}", simplify = TRUE)
the_names_1 


## ---- -------------------------------------------------------------------------------
cat(substr(the_names_2, 1, options()$width))
cat(substr(the_names_2, options()$width, nchar(the_names_2)))


## ----------------------------------------------------------------------------------------------
the_names_2 <- the_names_2 |>
  str_trim() |>
  str_split("\\s+", simplify = TRUE)
the_names_2


## ----------------------------------------------------------------------------------------------
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) |>
  str_to_lower() |>
  str_replace_all("\\s", "_")
the_names


## ----------------------------------------------------------------------------------------------
new_research_funding_rates <- tab[6:14] |>
  str_trim() |>
  str_split("\\s{2,}", simplify = TRUE) |>
  data.frame() |>
  setNames(the_names) |>
  mutate_at(-1, parse_number)
new_research_funding_rates |> as_tibble()


## ----------------------------------------------------------------------------------------------
identical(research_funding_rates, new_research_funding_rates)



###############
## Recoding
###############


## ----------------------------------------------------------------------------------------------
library(dslabs)
data("gapminder")


## ----caribbean---------------------------------------------------------------------------------
gapminder |> 
  filter(region == "Caribbean") |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


## ----------------------------------------------------------------------------------------------
gapminder |> 
  filter(region == "Caribbean") |>
  filter(str_length(country) >= 12) |>
  distinct(country) 


## ----caribbean-with-nicknames------------------------------------------------------------------
gapminder |> filter(region=="Caribbean") |>
  mutate(country = recode(country, 
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

