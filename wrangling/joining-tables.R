## ----setup, include=FALSE-----------------------------------------------------------
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


## ---- warning=FALSE, message=FALSE--------------------------------------------------


## Joining tables

library(tidyverse) 
library(dslabs) 
data(murders) 
head(murders) 


## -----------------------------------------------------------------------------------
data(polls_us_election_2016) 
head(results_us_election_2016) 


## -----------------------------------------------------------------------------------
identical(results_us_election_2016$state, murders$state) 


## -----------------------------------------------------------------------------------


## Joins

tab <- left_join(murders, results_us_election_2016, by = "state") |> 
  select(-others) |> rename(ev = electoral_votes) 
head(tab) 


## ----ev-vs-population, message=FALSE, warning=FALSE, eval=FALSE---------------------
## library(ggrepel)
## tab |> ggplot(aes(population/10^6, ev, label = abb)) +
##   geom_point() +
##   geom_text_repel() +
##   scale_x_continuous(trans = "log2") +
##   scale_y_continuous(trans = "log2") +
##   geom_smooth(method = "lm", se = FALSE)


## ----ev-vs-population-code, message=FALSE, warning=FALSE, echo=FALSE----------------
library(ggrepel) 
tab |> ggplot(aes(population/10^6, ev, label = abb)) + 
  geom_point() + 
  geom_text_repel() +  
  scale_x_continuous(trans = "log2") + 
  scale_y_continuous(trans = "log2") + 
  geom_smooth(method = "lm", se = FALSE) 


## -----------------------------------------------------------------------------------
tab_1 <- slice(murders, 1:6) |> select(state, population) 
tab_1 


## -----------------------------------------------------------------------------------
tab_2 <- results_us_election_2016 |>  
  filter(state%in%c("Alabama", "Alaska", "Arizona",  
                    "California", "Connecticut", "Delaware")) |>  
  select(state, electoral_votes) |> rename(ev = electoral_votes) 
tab_2 


## -----------------------------------------------------------------------------------
left_join(tab_1, tab_2, by = "state") 


## ---- eval=FALSE--------------------------------------------------------------------
## tab_1 |> left_join(tab_2, by = "state")


## -----------------------------------------------------------------------------------
tab_1 |> right_join(tab_2, by = "state") 


## -----------------------------------------------------------------------------------
inner_join(tab_1, tab_2, by = "state") 


## -----------------------------------------------------------------------------------
full_join(tab_1, tab_2, by = "state") 


## -----------------------------------------------------------------------------------
semi_join(tab_1, tab_2, by = "state") 


## -----------------------------------------------------------------------------------
anti_join(tab_1, tab_2, by = "state") 


## ---- echo=FALSE, out.width = "40%"-------------------------------------------------
knitr::include_graphics(file.path(img_path,"joins.png")) 


## -----------------------------------------------------------------------------------
bind_cols(a = 1:3, b = 4:6) 


## -----------------------------------------------------------------------------------
tab_1 <- tab[, 1:3] 
tab_2 <- tab[, 4:6] 
tab_3 <- tab[, 7:8] 
new_tab <- bind_cols(tab_1, tab_2, tab_3) 
head(new_tab) 


## -----------------------------------------------------------------------------------
tab_1 <- tab[1:2,] 
tab_2 <- tab[3:4,] 
bind_rows(tab_1, tab_2) 


## -----------------------------------------------------------------------------------
intersect(1:10, 6:15) 


## -----------------------------------------------------------------------------------
intersect(c("a","b","c"), c("b","c","d")) 


## -----------------------------------------------------------------------------------
tab_1 <- tab[1:5,] 
tab_2 <- tab[3:7,] 
dplyr::intersect(tab_1, tab_2) 


## -----------------------------------------------------------------------------------
union(1:10, 6:15) 
union(c("a","b","c"), c("b","c","d")) 


## -----------------------------------------------------------------------------------
tab_1 <- tab[1:5,] 
tab_2 <- tab[3:7,] 
dplyr::union(tab_1, tab_2)  


## -----------------------------------------------------------------------------------
setdiff(1:10, 6:15) 
setdiff(6:15, 1:10) 


## -----------------------------------------------------------------------------------
tab_1 <- tab[1:5,] 
tab_2 <- tab[3:7,] 
dplyr::setdiff(tab_1, tab_2) 


## -----------------------------------------------------------------------------------
setequal(1:5, 1:6) 


## -----------------------------------------------------------------------------------
setequal(1:5, 5:1) 


## -----------------------------------------------------------------------------------
dplyr::setequal(tab_1, tab_2) 

