## ----setup, include=FALSE---------------------------------------------------------------------------------
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


## ---- warning=FALSE, message=FALSE------------------------------------------------------------------------
library(tidyverse) 
library(dslabs) 
data("movielens") 


## ---------------------------------------------------------------------------------------------------------
movielens |> as_tibble() 


## ---------------------------------------------------------------------------------------------------------
movielens |>  
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId)) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
keep <- movielens |>  
  count(movieId) |>  
  top_n(4, n) |>  
  pull(movieId) 
tab <- movielens |>  
  filter(movieId%in%keep) |>  
  filter(userId %in% c(13:20)) |>  
  select(userId, title, rating) |>  
  mutate(title = str_remove(title, ", The"), 
         title = str_remove(title, ":.*")) |> 
  pivot_wider(names_from="title", values_from="rating") 
knitr::kable(tab)


## ----sparsity-of-movie-recs, echo=FALSE, fig.width=3, fig.asp=1, out.width="50%"--------------------------
users <- sample(unique(movielens$userId), 100) 
rafalib::mypar() 
movielens |>  
  filter(userId %in% users) |>  
  select(userId, movieId, rating) |> 
  mutate(rating = 1) |> 
  pivot_wider(names_from = movieId, values_from = rating) |>  
  (\(mat) mat[, sample(ncol(mat), 100)])() |> 
  as.matrix() |>  
  t() |> 
  image(1:100, 1:100, z = _ , xlab="Movies", ylab="Users") 


## ----movie-id-and-user-hists, echo=FALSE, fig.width=6, fig.height=3---------------------------------------
p1 <- movielens |>  
  count(movieId) |>  
  ggplot(aes(n)) +  
  geom_histogram(bins = 30, color = "black") +  
  scale_x_log10() +  
  ggtitle("Movies") 
p2 <- movielens |>  
  count(userId) |>  
  ggplot(aes(n)) +  
  geom_histogram(bins = 30, color = "black") +  
  scale_x_log10() +  
  ggtitle("Users") 
gridExtra::grid.arrange(p1, p2, ncol = 2) 


## ---- message=FALSE, warning=FALSE------------------------------------------------------------------------
dat <- movielens |> group_by(movieId) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n >= 5) |>  
  group_by(userId) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n >= 100) |>
  select(-n)  


## ---- message=FALSE, warning=FALSE------------------------------------------------------------------------
set.seed(2006) 
indexes <- split(1:nrow(dat), dat$userId) 
test_ind <- sapply(indexes, function(ind){
  sample(ind, ceiling(length(ind)*.2))}) |> 
  unlist(use.names = TRUE) |> sort() 
test_set <- dat[test_ind,] 
train_set <- dat[-test_ind,] 


## ---------------------------------------------------------------------------------------------------------
test_set <- test_set |>  
  semi_join(train_set, by = "movieId") |> 
  semi_join(train_set, by = "userId") 


## ---------------------------------------------------------------------------------------------------------
mat <- select(train_set, movieId, userId, rating) |> 
  pivot_wider(names_from = movieId, values_from = rating)  
rnames <- mat$userId 
mat <- as.matrix(mat[,-1]) 
rownames(mat) <- rnames 


## ---------------------------------------------------------------------------------------------------------
movie_map <- train_set |> select(movieId, title) |>
  distinct(movieId, .keep_all = TRUE) 


## ---------------------------------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){ 
    sqrt(mean((true_ratings - predicted_ratings)^2)) 
  } 


## ---------------------------------------------------------------------------------------------------------
mu <- mean(mat, na.rm = TRUE) 
mu 


## ---------------------------------------------------------------------------------------------------------
naive_rmse <- RMSE(test_set$rating, mu) 
naive_rmse 


## ---------------------------------------------------------------------------------------------------------
predictions <- rep(3, nrow(test_set)) 
RMSE(test_set$rating, predictions) 


## ---------------------------------------------------------------------------------------------------------
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse) 


## ---- eval=FALSE------------------------------------------------------------------------------------------
#> fit <- lm(rating ~ as.factor(movieId), data = movielens)


## ---------------------------------------------------------------------------------------------------------
b_i <- colMeans(mat - mu, na.rm = TRUE) 


## ----movie-effects, eval=FALSE----------------------------------------------------------------------------
#> qplot(b_i, bins = 10, color = I("black"))


## ----movie-effects-run, echo=FALSE, out.width="50%"-------------------------------------------------------
qplot(b_i, bins = 10, color = I("black")) 


## ---------------------------------------------------------------------------------------------------------
fit_movies <- data.frame(movieId = as.integer(colnames(mat)),  
                         mu = mu, b_i = b_i) 
left_join(test_set, fit_movies, by = "movieId") |>  
  mutate(pred = mu + b_i) |>  
  summarize(rmse = RMSE(rating, pred)) 


## ----echo=FALSE-------------------------------------------------------------------------------------------
model_1_rmse <- left_join(test_set, fit_movies, by = "movieId") |>  
  mutate(pred = mu + b_i) |>  
  summarize(rmse = RMSE(rating, pred)) |> 
  pull(rmse) 
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Movie Effect Model",   
                                 RMSE = model_1_rmse)) 


## ----user-effect-hist, eval=FALSE-------------------------------------------------------------------------
#> b_u <- rowMeans(mat, na.rm = TRUE)
#> qplot(b_u, bins = 30, color = I("black"))


## ----user-effect-hist-run, echo=FALSE---------------------------------------------------------------------
b_u <- rowMeans(mat, na.rm = TRUE) 
qplot(b_u, bins = 30, color = I("black")) 


## ---- eval = FALSE----------------------------------------------------------------------------------------
#> lm(rating ~ as.factor(movieId) + as.factor(userId))


## ---------------------------------------------------------------------------------------------------------
b_u <- rowMeans(sweep(mat - mu, 2, b_i), na.rm = TRUE) 


## ---------------------------------------------------------------------------------------------------------
fit_users <- data.frame(userId = as.integer(rownames(mat)), b_u = b_u) 
left_join(test_set, fit_movies, by = "movieId") |>  
  left_join(fit_users, by = "userId") |>  
  mutate(pred = mu + b_i + b_u) |>  
  summarize(rmse = RMSE(rating, pred)) 


## ----echo=FALSE-------------------------------------------------------------------------------------------
model_2_rmse <- left_join(test_set, fit_movies, by = "movieId") |>  
  left_join(fit_users, by = "userId") |>  
  mutate(pred = mu + b_i + b_u) |>  
  summarize(rmse = RMSE(rating, pred)) |>  
  pull(rmse) 
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Movie + User Effects Model",   
                                 RMSE = model_2_rmse)) 


## ---------------------------------------------------------------------------------------------------------
n <-  colSums(!is.na(mat)) 
fit_movies$n <- n 
best <- fit_movies |> left_join(movie_map, by = "movieId") |>  
  top_n(3, b_i) |>  
  arrange(desc(b_i)) |> 
  mutate(average_rating = mu + b_i) 
best |>  select(title, average_rating, n)  


## ---------------------------------------------------------------------------------------------------------
test_set |>  
  group_by(movieId) |> 
  summarize(averge_rating_test = mean(rating)) |> 
  right_join(best, by = "movieId") |> 
  select(title, average_rating, averge_rating_test, n)  


## ---------------------------------------------------------------------------------------------------------
lambdas <- seq(0, 10, 0.25) 
rmses <- sapply(lambdas, function(lambda){ 
  b_i <- colSums(mat - mu, na.rm = TRUE) / (n + lambda) 
  fit <- data.frame(movieId = as.integer(colnames(mat)), mu = mu, b_i = b_i) 
  left_join(test_set, fit, by = "movieId") |> mutate(pred = mu + b_i) |>  
    summarize(rmse = RMSE(rating, pred)) |> 
    pull(rmse) 
}) 


## ----best-penalty, eval=FALSE-----------------------------------------------------------------------------
#> qplot(lambdas, rmses)
#> lambda <- lambdas[which.min(rmses)]
#> print(lambda)


## ----best-penalty-run, echo=FALSE-------------------------------------------------------------------------
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)] 
print(lambda) 


## ---------------------------------------------------------------------------------------------------------
b_i <- colSums(mat - mu, na.rm = TRUE) / (n + lambda) 


## ----regularization-shrinkage, echo=FALSE-----------------------------------------------------------------
tibble(original = fit_movies$b_i,  
       regularlized = b_i,  
       n = n) |> 
  ggplot(aes(original, regularlized, size=sqrt(n))) +  
  geom_point(shape=1, alpha=0.5) + 
  geom_abline() 


## ---- echo=FALSE------------------------------------------------------------------------------------------
fit_reg_movies <- data.frame(movieId = as.integer(colnames(mat)), mu = mu, b_i = b_i, n = n) 
best <- fit_reg_movies |> left_join(movie_map, by = "movieId") |> top_n(3, b_i) |>  
  arrange(desc(b_i)) |> 
  mutate(average_rating = mu + b_i) 
test_set |>  
  group_by(movieId) |> 
  summarize(averge_rating_test = mean(rating)) |> 
  right_join(best, by = "movieId") |> 
  select(title, average_rating, averge_rating_test, n)  


## ---------------------------------------------------------------------------------------------------------
fit_users$b_u <- rowMeans(sweep(mat - mu, 2, b_i), na.rm = TRUE) 
left_join(test_set, fit_reg_movies, by = "movieId") |>  
  left_join(fit_users, by = "userId") |>  
  mutate(pred = mu + b_i + b_u) |>  
  summarize(rmse = RMSE(rating, pred)) 


## ----echo=FALSE-------------------------------------------------------------------------------------------
model_3_rmse <- left_join(test_set, fit_reg_movies, by = "movieId") |>  
  left_join(fit_users, by = "userId") |>  
  mutate(pred = mu + b_i + b_u) |>  
  summarize(rmse = RMSE(rating, pred)) |>  
  pull(rmse) 
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Regularized Movie + User Effect Model",   
                                 RMSE = model_3_rmse)) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
knitr::kable(rmse_results)


## ---------------------------------------------------------------------------------------------------------
r <- sweep(mat - mu, 2, fit_reg_movies$b_i) - fit_users$b_u 
colnames(r) <- with(movie_map, title[match(colnames(r), movieId)]) 


## ----movie-cor, echo=FALSE, warning=FALSE, message=FALSE, out.width="100%", fig.width=9, fig.asp=.33------
m_1 <- "Godfather, The" 
m_2 <- "Godfather: Part II, The" 
p1 <- qplot(r[ ,m_1], r[,m_2], xlab = m_1, ylab = m_2) 
m_3 <- "Goodfellas" 
p2 <- qplot(r[ ,m_1], r[,m_3], xlab = m_1, ylab = m_3) 
m_4 <- "You've Got Mail"  
m_5 <- "Sleepless in Seattle"  
p3 <- qplot(r[ ,m_4], r[,m_5], xlab = m_4, ylab = m_5) 
gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
x <- r[, c(m_1, m_2, m_3, m_4, m_5)] 
short_names <- c("Godfather", "Godfather2", "Goodfellas", 
                 "You've Got", "Sleepless") 
colnames(x) <- short_names 
cor(x, use="pairwise.complete") 


## ---- echo=FALSE------------------------------------------------------------------------------------------
q <- matrix(c(1 , 1, 1, -1, -1), ncol = 1) 
rownames(q) <- short_names 
p <- matrix(rep(c(2, 0, -2), c(3, 5, 4)), ncol = 1) 
rownames(p) <- 1:nrow(p) 
set.seed(1988) 
r <- jitter(p %*% t(q)) 


## ---------------------------------------------------------------------------------------------------------
round(r, 1) 


## ---------------------------------------------------------------------------------------------------------
cor(r)  


## ---------------------------------------------------------------------------------------------------------
t(q)  


## ---------------------------------------------------------------------------------------------------------
t(p) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
set.seed(1988) 
m_6 <- "Scent of a Woman" 
q <- cbind(c(1 , 1, 1, -1, -1, -1),  
           c(1 , 1, -1, -1, -1, 1)) 
rownames(q) <- c(short_names, "Scent") 
p <- cbind(rep(c(2,0,-2), c(3,5,4)),  
          c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2 
rownames(p) <- 1:nrow(p) 
r <- jitter(p %*% t(q), factor=1) 


## ---------------------------------------------------------------------------------------------------------
round(r, 1) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
colnames(r)[4:6] <- c("YGM", "SS", "SW") 
cor(r) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
cnames <- colnames(r) 
r <- sweep(mat - mu, 2, fit_reg_movies$b_i) - fit_users$b_u 
colnames(r) <- with(movie_map, title[match(colnames(r), movieId)]) 
six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6) 
x <- r[, six_movies] 
colnames(x) <- cnames 
cor(x, use="pairwise.complete") 


## ---------------------------------------------------------------------------------------------------------
t(q)  


## ---------------------------------------------------------------------------------------------------------
t(p) 


## ---------------------------------------------------------------------------------------------------------
keep <- c("Godfather, The", "Godfather: Part II, The", "Goodfellas", "Ghost", "Titanic",  
          "Scent of a Woman") 
dat <- movielens  |>  
  group_by(userId) |> 
  filter(n() >= 250) |>  
  ungroup() |> 
  group_by(movieId) |> 
  filter(n() >= 50 | title %in% keep) |>  
  ungroup()  
y <- select(dat, movieId, userId, rating) |> 
  pivot_wider(names_from = movieId, values_from = rating)  
y <- as.matrix(y[,-1]) 
colnames(y) <- dat |> select(movieId, title) |>  
  distinct(movieId, .keep_all = TRUE) |> 
  right_join(data.frame(movieId=as.integer(colnames(y))), by = "movieId") |> 
  pull(title) 


## ---------------------------------------------------------------------------------------------------------
r <- sweep(y, 2, colMeans(y, na.rm=TRUE)) 


## ---------------------------------------------------------------------------------------------------------
r[is.na(r)] <- -1 


## ---------------------------------------------------------------------------------------------------------
r <- r - rowMeans(r) 


## ---------------------------------------------------------------------------------------------------------
pca <- prcomp(r) 


## ---------------------------------------------------------------------------------------------------------
dim(pca$rotation) 


## ---------------------------------------------------------------------------------------------------------
dim(pca$x) 


## ---- pca-sds, eval=FALSE---------------------------------------------------------------------------------
#> qplot(1:nrow(pca$x), pca$sdev, xlab = "PC")


## ----pca-sds-run, echo=FALSE------------------------------------------------------------------------------
qplot(1:nrow(pca$x), pca$sdev, xlab = "PC") 


## ----var-expained-pca, eval=FALSE-------------------------------------------------------------------------
#> var_explained <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
#> qplot(1:nrow(pca$x), var_explained, xlab = "PC")  + ylim(c(0,1))


## ----var-expained-pca-run, echo=FALSE---------------------------------------------------------------------
var_explained <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
qplot(1:nrow(pca$x), var_explained, xlab = "PC") + ylim(c(0,1))


## ----movies-pca, echo=FALSE, warning=FALSE, out.width="100%"----------------------------------------------
library(ggrepel) 
pcs <- data.frame(pca$rotation, name = str_trunc(colnames(y), 30),  
                  stringsAsFactors = FALSE) 
highlight <- filter(pcs, PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1) 
pcs |>  ggplot(aes(PC1, PC2)) + geom_point() +  
  geom_text_repel(aes(PC1, PC2, label=name), 
                  data = highlight, size = 2) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
pcs |> select(name, PC1) |> arrange(PC1) |> slice(1:10) |> pull(name) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
pcs |> select(name, PC1) |> arrange(desc(PC1)) |> slice(1:10) |> pull(name) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
pcs |> select(name, PC2) |> arrange(PC2) |> slice(1:10) |> pull(name) 


## ---- echo=FALSE------------------------------------------------------------------------------------------
pcs |> select(name, PC2) |> arrange(desc(PC2)) |> slice(1:10) |> pull(name) 

