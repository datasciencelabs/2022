## ----setup, include=FALSE------------------------------------
library(tidyverse)
library(dslabs)
library(gridExtra)
library(ggthemes)
ds_theme_set()
options(digits = 3)
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

img_path <- "img"


## ---- message=FALSE, warning=FALSE---------------------------
library(tidyverse)  
library(dslabs) 
path <- system.file("extdata", package="dslabs") 
filename <- file.path(path, "fertility-two-countries-example.csv") 
wide_data <- read_csv(filename) 


## ------------------------------------------------------------
new_tidy_data <- pivot_longer(wide_data, `1960`:`2015`, names_to = "year", values_to = "fertility") 


## ------------------------------------------------------------
new_tidy_data <- wide_data |>  
  pivot_longer(`1960`:`2015`, names_to = "year", values_to = "fertility") 


## ------------------------------------------------------------
head(new_tidy_data) 


## ------------------------------------------------------------
new_tidy_data <- wide_data |> 
  pivot_longer(-country, names_to = "year", values_to = "fertility") 


## ------------------------------------------------------------
data("gapminder") 
tidy_data <- gapminder |>  
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) |> 
  select(country, year, fertility) 


## ------------------------------------------------------------
class(tidy_data$year) 
class(new_tidy_data$year) 


## ------------------------------------------------------------
new_tidy_data <- wide_data |> 
  pivot_longer(-country, names_to = "year", values_to = "fertility") |> 
  mutate(year = as.integer(year)) 


## ----fertility-year-check, eval=FALSE------------------------
## new_tidy_data |> ggplot(aes(year, fertility, color = country)) +
##   geom_point()
## 
## 
## ## `pivot_longer`
## 


## ------------------------------------------------------------
new_wide_data <- new_tidy_data |>  
  pivot_wider(names_from = year, values_from = fertility) 
select(new_wide_data, country, `1960`:`1967`) 


## ---- echo=FALSE---------------------------------------------
knitr::include_graphics(file.path(img_path,"gather-spread.png")) 


## ---- message=FALSE------------------------------------------
path <- system.file("extdata", package = "dslabs") 
filename <- "life-expectancy-and-fertility-two-countries-example.csv" 
filename <-  file.path(path, filename) 
raw_dat <- read_csv(filename) 
select(raw_dat, 1:5) 


## ------------------------------------------------------------
dat <- raw_dat |> pivot_longer(-country) 
head(dat) 


## ------------------------------------------------------------
dat$name[1:5] 


## ---- eval=FALSE---------------------------------------------
## dat |> separate(name, c("year", "name"), "_")


## ------------------------------------------------------------
dat |> separate(name, c("year", "name")) 


## ------------------------------------------------------------
var_names <- c("year", "first_variable_name", "second_variable_name") 
dat |> separate(name, var_names, fill = "right") 


## ------------------------------------------------------------
dat |> separate(name, c("year", "name"), extra = "merge") 


## ------------------------------------------------------------
dat |>  
  separate(name, c("year", "name"), extra = "merge") |> 
  pivot_wider() 


## ------------------------------------------------------------
var_names <- c("year", "first_variable_name", "second_variable_name") 
dat |>  
  separate(name, var_names, fill = "right") 


## ------------------------------------------------------------
dat |>  
  separate(name, var_names, fill = "right") |> 
  unite(name, first_variable_name, second_variable_name) |> 
  pivot_wider() |> 
  rename(fertility = fertility_NA) 

