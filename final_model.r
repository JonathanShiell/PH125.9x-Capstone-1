############# Preamble ##########
#
# A simplified script that produces the final model. This runs the edx-supplied 
# code so as to provide the required data objects `validation` and `edx`.

########### Sample code from EdX ##########

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

########### Beginning of own code ############

#Load Professor Irizarry's RMSE function:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Compute population mean mu (for training set).
mu <- mean(edx$rating)

#Compute possible values of lambda, stored as `lambdas`:
lambdas <- seq(0, 10, 0.25)

#  Select value of lambda from object `lambdas`, based on Chapter 34 of
# Irizarry (2019)
rmses_movie_User <- sapply(lambdas, function(l){
  
  # Comptes movie bias effects for each value of `l` (lambda)
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #Compute user bias effects for each value of `l` (lambda)
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #Compute predictions for each value of `l` (lambda)
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  #Determine and return RMSE for given value of Lambda.
  return(RMSE(validation$rating,predicted_ratings))
})

#Plot RMSEs for various values of lambda
qplot(lambdas,rmses_movie_User)

#Save value of lambda that produces lowest RMSE and print to console.
lambda_movie_user <- lambdas[which.min(rmses_movie_User)]
lambda_movie_user

#Create objects for model

#  Compute the movie biases. This retains additional information that may be
# used by downstream filtering systems.
b_i <- edx %>% 
  group_by(movieId,title,genres) %>%
  summarize(b_i_reg = sum(rating - mu)/(n()+lambda_movie_user),n_movie = n()) %>%
  ungroup()
# The purpose of the above line is to ease downstream use.

#Compute the user biases
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating - b_i_reg - mu)/(n()+lambda_movie_user))

#Apply to test set. Save predicted values in memory as `predicted_ratings`.
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i_reg + b_u_reg) %>%
  pull(pred)

#Determine final RMSE and print to console, saving to memory as `final_rmse`.
final_rmse <- RMSE(validation$rating,predicted_ratings)
final_rmse