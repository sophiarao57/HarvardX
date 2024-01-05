## ---- message=FALSE,warning = FALSE-----------------------------
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(caret)
library(Hmisc)
library(data.table)
library(recommenderlab)
library(vioplot) 
library(plyr)
library(plotly)
library(hrbrthemes)
library(Metrics)
library(lubridate)
library(recosystem)


## ---------------------------------------------------------------
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
 
rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ---- echo = FALSE,warning = FALSE------------------------------
head(edx)


## ---- echo = FALSE,warning = FALSE------------------------------
# Convert timestamp to a human readable date

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")

# Extract the year and month the rating was given
edx$yearOfRating <- format(edx$date,"%Y")
edx$monthOfRating <- format(edx$date,"%m")


# Extract the year of release for each movie from the title column

edx <- edx %>%
   mutate(title = str_trim(title)) %>%
   extract(title,
           c("titleTemp", "release"),
           regex = "^(.*) \\(([0-9 \\-]*)\\)$",
           remove = F) %>%
   mutate(release = if_else(str_length(release) > 4,
                                as.integer(str_split(release, "-",
                                                     simplify = T)[1]),
                                as.integer(release))
   ) %>%
   mutate(title = if_else(is.na(titleTemp),
                          title,
                          titleTemp)
         ) %>%
  select(-titleTemp)



## ---- echo=FALSE,warning = FALSE--------------------------------
head(edx)


## ----message = FALSE,warning = FALSE----------------------------
# Extract the genre in edx dataset

edx <- edx %>%
   mutate(genre = fct_explicit_na(genres,
                                       na_level = "(no genres listed)")
          ) %>%
   separate_rows(genre,
                 sep = "\\|")


## ----warning = FALSE--------------------------------------------
edx <- edx %>% mutate(yearsSinceRelsease = as.numeric(yearOfRating)-as.numeric(release))


## ---- echo = FALSE,warning = FALSE------------------------------
edxplot <- ggplot(edx, aes(userId, movieId, fill= rating)) + 
  geom_tile() +
  theme_ipsum() 

edxplot


## ---- echo = FALSE,warning = FALSE------------------------------
ggplot(edx) + geom_histogram(mapping = aes(x = rating)) 


## ---- echo=FALSE,warning = FALSE--------------------------------
meanRatings <- edx %>% group_by(movieId) %>% summarise(meanRating = mean(rating))

ggplot(meanRatings) + geom_histogram(mapping = aes(x = meanRating))


## ---- echo=FALSE,warning = FALSE--------------------------------

perUser <- edx %>% group_by(userId) %>% summarise(ratingsGiven = n())

ggplot(perUser) + geom_histogram(mapping = aes (x = ratingsGiven)) 



## ---- echo=FALSE,warning = FALSE--------------------------------
meanUserRatings <- edx %>% group_by(userId) %>% summarise(meanUserRating = mean(rating))

ggplot(meanUserRatings) + geom_histogram(mapping = aes (x = meanUserRating)) 


## ----warning = FALSE--------------------------------------------
#RMSE function
RMSE <- function(test, train){
  sqrt(mean((test - train)^2))
}


## ----warning = FALSE--------------------------------------------
mu = mean(edx$rating)


## ----warning = FALSE--------------------------------------------

naive_rmse <- rmse(final_holdout_test$rating, mu)
naive_rmse


## ----warning = FALSE--------------------------------------------
rmse_results <- tibble(Method = "Model 1: Simple overall average model", RMSE = naive_rmse)
rmse_results


## ----warning = FALSE--------------------------------------------
age_effect<- edx %>% 
  group_by(yearsSinceRelsease) %>%
  summarise(b_a = mean(rating)-mu)
head(age_effect)

age_effect %>% qplot(b_a, geom ="histogram",  data = .)


## ----warning = FALSE--------------------------------------------
model_2_rmse <- RMSE(final_holdout_test$rating,mu) # 1.05239
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Age Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results


## ----warning = FALSE--------------------------------------------
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = .)

head(movie_avgs)


## ----warning = FALSE--------------------------------------------
predicted_ratings_3 <- mu + final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_3_rmse <- RMSE(final_holdout_test$rating,na.omit(predicted_ratings_3))
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie Effect Model",  
                                     RMSE = model_3_rmse))
rmse_results


## ----warning = FALSE--------------------------------------------
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

#head(user_avgs)

predicted_ratings_4 <- final_holdout_test %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_4_rmse <- RMSE(final_holdout_test$rating,na.omit(predicted_ratings_4))
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User Effects Model",
                                     RMSE = model_4_rmse))
rmse_results


## ----warning = FALSE--------------------------------------------
# use 10-fold cross validation to pick a lambda for movie effects regularization
# split the data into 10 parts
set.seed(2019, sample.kind = "Rounding")
cv_splits <- createFolds(edx$rating, k=10, returnTrain =TRUE)

# define a matrix to store the results of cross validation
rmses <- matrix(nrow=10,ncol=51)
lambdas <- seq(0, 5, 0.1)

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  just_the_sum <- train_final %>% 
    group_by(movieId) %>% 
    summarise(s = sum(rating - mu), n_i = n())
  
  rmses[k,] <- sapply(lambdas, function(l){
    predicted_ratings <- test_final %>% 
      left_join(just_the_sum, by='movieId') %>% 
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}

rmses_cv <- colMeans(rmses)
qplot(lambdas,rmses_cv)
lambda <- lambdas[which.min(rmses_cv)]
lambda #2.2


## ---------------------------------------------------------------
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
predicted_ratings_5 <- final_holdout_test %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
model_5_rmse <- RMSE(na.omit(predicted_ratings_5), final_holdout_test$rating)   # 0.943852 not too much improved
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie Effect Model",  
                                     RMSE = model_5_rmse))
rmse_results 


## ----warning = FALSE--------------------------------------------
# define a matrix to store the results of cross validation
lambdas <- seq(0, 8, 0.1)
rmses_2 <- matrix(nrow=10,ncol=length(lambdas))
# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_2[k,] <- sapply(lambdas, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarise(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}

rmses_2
rmses_2_cv <- colMeans(rmses_2)
rmses_2_cv
qplot(lambdas,rmses_2_cv)
lambda <- lambdas[which.min(rmses_2_cv)]   #4.9


## ----warning = FALSE--------------------------------------------
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+lambda))
b_u_reg <- edx %>% 
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings_6 <- 
    final_holdout_test %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
model_6_rmse <- RMSE(na.omit(predicted_ratings_6), final_holdout_test$rating)   # 0.864818
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie + User Effect Model",  
                                     RMSE = model_6_rmse))
rmse_results 


## ----warning = FALSE--------------------------------------------
# define a matrix to store the results of cross validation
lambda_i <- 2.2 
lambdas_u <- seq(0, 8, 0.1)
rmses_3 <- matrix(nrow=10,ncol=length(lambdas_u))

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_3[k,] <- sapply(lambdas_u, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarise(b_i = sum(rating - mu)/(n()+lambda_i))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}
#rmses_3
rmses_3_cv <- colMeans(rmses_3)
#rmses_3_cv
qplot(lambdas_u,rmses_3_cv)
lambda_u <-lambdas_u[which.min(rmses_3_cv)]   #5
lambda_u


## ----warning = FALSE--------------------------------------------
lambda_i <- 2.2
lambda_u <- 5
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+lambda_i))
b_u_reg <- edx %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
predicted_ratings_7 <- 
  final_holdout_test %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_7_rmse <- RMSE(na.omit(predicted_ratings_7), final_holdout_test$rating)   # 0.86485
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie + User Effect Model Version 2",  
                                     RMSE = model_7_rmse))
rmse_results 


## ----warning = FALSE--------------------------------------------
# define a matrix to store the results of cross validation
lambda_u <- 5 
lambdas_i <- seq(0, 8, 0.1)
rmses_3 <- matrix(nrow=10,ncol=length(lambdas_u))

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_3[k,] <- sapply(lambdas_u, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarise(b_i = sum(rating - mu)/(n()+lambda_i))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}
#rmses_3
rmses_3_cv <- colMeans(rmses_3)
#rmses_3_cv
qplot(lambdas_u,rmses_3_cv)
lambda_i <-lambdas_i[which.min(rmses_3_cv)]   #4.6
lambda_i


## ----warning = FALSE--------------------------------------------
lambda_i <- 4.6
lambda_u <- 5
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+lambda_i))
b_u_reg <- edx %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
predicted_ratings_7 <- 
  final_holdout_test %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_7_rmse <- RMSE(na.omit(predicted_ratings_7), final_holdout_test$rating)   # 0.86485
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie + User Effect Model Version 3",  
                                     RMSE = model_7_rmse))
rmse_results 


## ----warning = FALSE--------------------------------------------
lambda <- 4.9
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+lambda))
b_u_reg <- edx %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings_6_edx <- 
  edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_6_rmse_edx <- RMSE(na.omit(predicted_ratings_6_edx), edx$rating)
model_6_rmse_edx


## ----warning = FALSE--------------------------------------------
edx_residual <- edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(residual = rating - mu - b_i - b_u) %>%
  select(userId, movieId, residual)
head(edx_residual)


## ----warning = FALSE--------------------------------------------
# as matrix
edx_for_mf <- as.matrix(edx_residual)
validation_for_mf <- final_holdout_test %>% 
  select(userId, movieId, rating)
validation_for_mf <- as.matrix(validation_for_mf)

# write edx_for_mf and validation_for_mf tables on disk
write.table(edx_for_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_for_mf, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# use data_file() to specify a data set from a file in the hard disk.
set.seed(2019) 
train_set <- data_file("trainset.txt")
valid_set <- data_file("validset.txt")

# build a recommender object
r <-Reco()

# tuning training set
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts


## ----warning = FALSE--------------------------------------------
# training the recommender model
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
pred_file <- tempfile()
r$predict(valid_set, out_file(pred_file))  
predicted_residuals_mf <- scan(pred_file)
predicted_ratings_mf <- predicted_ratings_6 + predicted_residuals_mf
rmse_mf <- RMSE(na.omit(predicted_ratings_mf),final_holdout_test$rating) # 0.786256
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Matrix Factorization",  
                                     RMSE = rmse_mf))
rmse_results 


## ---- echo=FALSE,warning = FALSE--------------------------------
rmse_results

