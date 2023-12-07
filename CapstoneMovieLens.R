library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(ggrepel)
library(dslabs)
library(lubridate)

options(digits = 3)
options(timeout = 120)
dl <- "ml-10M100K.zip"
ratings_file <- "ml-10M100K/ratings.dat"
movies_file <- "ml-10M100K/movies.dat"
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
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Analysis
# Checking data
class(edx)
dim(edx)[1]
dim(final_holdout_test)[1]
str(edx)

# Checking unique values for user id, movie ids, titles
n_distinct(edx$userId)
n_distinct(edx$movieId) 
n_distinct(edx$title) 

# Identified two movies with same title
duplicateMovieTitle <- edx %>% select(movieId, title) %>% unique() %>% 
  group_by(title) %>% 
  summarize(n=n()) %>%
  filter(n>1)

edx %>% filter(title==duplicateMovieTitle$title) %>%
  select(movieId, genres) %>%
  group_by(movieId, genres) %>% 
  summarize(n=n()) %>%
  unique()

# Extract all unique genres
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

n_distinct(genres) # Unique genres
n_distinct(edx$genres) # Unique genres combinations

# Data cleaning
# Converting to date format
edx <- edx %>% mutate(reviewDate = round_date(as_datetime(timestamp), unit = "day"))

# Extract movie release year
edx <- edx %>% mutate(title = str_trim(title)) %>%
  extract(title, c("shortTitle", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(year = as.integer(year)) %>%
  select(-shortTitle)

# Extract difference in years between the review date and the movie's release year
edx <- edx %>%
  mutate(reviewDelay = year(reviewDate)-year)

# Data visualisation
# Rating distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth=0.5, color="black") +
  scale_y_continuous() +
  labs(title = "Ratings distribution", x = "Rating value", y = "Count")

# Distribution of the average ratings for movies.
edx %>% group_by(movieId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins=50, color="black") +
  labs(x = "Mean rating", y = "Movie count")

# Distribution of the average ratings given by users.
edx %>% group_by(userId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins=50, color="black") +
  labs(x = "Mean rating", y = "User count")

#  Distribution of the number of ratings per user.
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins=50, color="black") +
  scale_x_log10() +
  labs(x = "User", y = "Ratings")

# List of individual genres, average by genre (not combination)
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

indiv_genres <- as.data.frame(genres)
names(indiv_genres) <- c("genre")

indiv_genres$n <- sapply(genres, function(g) {
  nrow(edx[str_detect(edx$genres, g), ])
})

indiv_genres$meanRating <- sapply(genres, function(g) {
  mean(edx[str_detect(edx$genres, g), "rating"])
})

indiv_genres$sd <- sapply(genres, function(g) {
  sd(edx[str_detect(edx$genres, g), "rating"])
})

indiv_genres$se <- indiv_genres$sd / sqrt(indiv_genres$n)
indiv_genres <- indiv_genres %>% arrange(desc(n))

indiv_genres %>% filter(genre!="(no genres listed)") %>%
  mutate(genre = reorder(genre, meanRating)) %>%
  ggplot(aes(x = genre, y = meanRating, ymin=meanRating - 2*se, ymax=meanRating + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre", y = "Average Rating")

# Average rating for movies across different release years
edx %>% group_by(year) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth(formula='y~x', method='loess', span = 0.15) +
  labs(x = "Release Year", y = "Average Rating")

# Distribution of the number of ratings across different release years.
edx %>% group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(year, n)) +
  geom_point() +
  scale_y_log10() +
  labs(x = "Release Year", y = "Rating Count")

# Average rating trend over time based on review dates.
edx %>% group_by(reviewDate) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(reviewDate, mean_rating)) +
  geom_point() +
  geom_smooth(formula='y~x', method='loess', span = 0.15) +
  labs(x = "Review Date", y = "Average Rating")

# Average rating of movies varies with different review delays.
edx %>% group_by(reviewDelay) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(reviewDelay, mean_rating)) +
  geom_point() +
  labs(x = "Review Delay", y = "Average Rating")

# Number of ratings distributed across different review delays.
edx %>% group_by(reviewDelay) %>%
  summarise(n = n()) %>%
  ggplot(aes(reviewDelay, n)) +
  geom_point() +
  scale_y_log10()+
  labs(x = "Review Delay", y = "Rating Count")

# Data Modelling
# Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm=TRUE))
}

set.seed(1)
# Create the Data Partition
d.index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
d.train <- edx[-d.index, ]
d.test <- edx[d.index, ]

# Remove index to clean up environment
rm(d.index)

# Check if d.train and d.test are data frames
if (!is.data.frame(d.train))
  stop("d.train is not a data frame.")

if (!is.data.frame(d.test))
  stop("d.test is not a data frame.")

mu_hat <- mean(d.train$rating)

# M01: simple recommendation model.
rmse.simple <- RMSE(d.test$rating, mu_hat)

# Model 02: Adding movie effect (movie_effect)

avg.movies <- d.train %>%
  group_by(movieId) %>%
  summarise(movie_effect = mean(rating - mu_hat))

predicted.movie_effect <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  mutate(pred = mu_hat + movie_effect) %>%
  pull(pred)

rmse.movie <- RMSE(d.test$rating, predicted.movie_effect)

# Model 03: Adding user effect
avg.users <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  group_by(userId) %>%
  summarise(user_effect = mean(rating - mu_hat - movie_effect))

predicted.user_effect <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  mutate(pred = mu_hat + movie_effect + user_effect) %>%
  pull(pred)

rmse.user <- RMSE(predicted.user_effect, d.test$rating)

# Model 04: Adding genre combination effect
avg.genres <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  group_by(genres) %>%
  summarise(genre_effect = mean(rating - mu_hat - movie_effect - user_effect))

predicted.genre_effect <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  mutate(pred = mu_hat + movie_effect + user_effect + genre_effect) %>%
  pull(pred)

rmse.genre <- RMSE(predicted.genre_effect, d.test$rating)

# Model 05: Adding movie release year effect
avg.years <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  group_by(year) %>%
  summarise(release_year_effect = mean(rating - mu_hat - movie_effect - user_effect - genre_effect))

predicted.release_year_effect <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by = "year") %>%
  mutate(pred = mu_hat + movie_effect + user_effect + genre_effect + release_year_effect) %>%
  pull(pred)

rmse.year <- RMSE(predicted.release_year_effect, d.test$rating)

# Model 06: Adding review delay effect
avg.delays <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by="year") %>%
  group_by(reviewDelay) %>%
  summarise(review_delay_effect = mean(rating - mu_hat - movie_effect - user_effect - genre_effect - release_year_effect))

predicted.review_delay_effect <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by = "year") %>%
  left_join(avg.delays, by = "reviewDelay") %>%
  mutate(pred = mu_hat + movie_effect + user_effect + genre_effect + release_year_effect + review_delay_effect) %>%
  pull(pred)

rmse.delay <- RMSE(predicted.review_delay_effect, d.test$rating)

# Model 07: Regularization of models.

# Generate a sequence of values for lambda ranging from 4 to 6 with 0.05 increments
inc <- 0.05
lambdas <- seq(4, 6, inc)

rmses <- sapply(lambdas, function(l){
  movie_effect <- d.train %>%
    group_by(movieId) %>%
    summarise(movie_effect = sum(rating - mu_hat)/(n()+l))
  user_effect <- d.train %>%
    left_join(movie_effect, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_effect = sum(rating - movie_effect - mu_hat)/(n()+l))
  genre_effect <- d.train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    group_by(genres) %>%
    summarise(genre_effect = sum(rating - movie_effect - user_effect - mu_hat)/(n()+l))
  release_year_effect <- d.train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(genre_effect, by="genres") %>%
    group_by(year) %>%
    summarise(release_year_effect = sum(rating - movie_effect - user_effect - genre_effect - mu_hat)/(n()+l))
  review_delay_effect <- d.train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(genre_effect, by="genres") %>%
    left_join(release_year_effect, by="year") %>%
    group_by(reviewDelay) %>%
    summarise(review_delay_effect = sum(rating - movie_effect - user_effect - genre_effect - release_year_effect - mu_hat)/(n()+l))
  predicted_ratings <- d.test %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(genre_effect, by="genres") %>%
    left_join(release_year_effect, by = "year") %>%
    left_join(review_delay_effect, by = "reviewDelay") %>%
    mutate(pred = mu_hat + movie_effect + user_effect + genre_effect + release_year_effect + review_delay_effect) %>%
    pull(pred)
  return(RMSE(predicted_ratings, d.test$rating))
})

# Get best Lambda and RMSE for regularized model
lambda <- lambdas[which.min(rmses)]
rmse.regularized <- min(rmses) 

# Plot Lambdas vs RMSE
lambda_rmse_data <- as.data.frame(lambdas)
lambda_rmse_data$rmses <- rmses
names(lambda_rmse_data) <- c("lambdas", "rmses")

ggplot(lambda_rmse_data, aes(lambdas, rmses)) +
  geom_point() + xlab("Lambda") + ylab("RMSE") +
  geom_label_repel(data=subset(lambda_rmse_data, lambdas == lambda), aes(label = lambdas), color = 'blue',
                   size = 3.5, box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))

# All results from models
rmse.results <- 
  tibble(Model = "Naive Prediction (Average)", RMSE = rmse.simple)
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Movie Effect", RMSE = rmse.movie))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ User Effect", RMSE = rmse.user))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Genre Combination Effect", RMSE = rmse.genre))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Release Year Effect", RMSE = rmse.year))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Review Delay Effect", RMSE = rmse.delay))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Regularized", RMSE = rmse.regularized))
rmse.results

# Data cleaning on final_holdout_test
# Convert timestamp into date format
final_holdout_test <- final_holdout_test %>% mutate(reviewDate = round_date(as_datetime(timestamp), unit = "week"))

# Removeing of release year in final_holdout_test
final_holdout_test <- final_holdout_test %>% mutate(title = str_trim(title)) %>%
  extract(title, c("shortTitle", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(year = as.integer(year)) %>%
  select(-shortTitle)

# Identify the delay between the movie review date and the movie's release year in final_holdout_test
final_holdout_test <- final_holdout_test %>%
  mutate(reviewDelay = year(reviewDate)-year)

# Data modelling on final_holdout_test
# Regularized model with movie, user, genre combination, release year, and review delay effects.
movie_effect <- edx %>%
  group_by(movieId) %>%
  summarise(movie_effect = sum(rating - mu_hat)/(n()+lambda))
user_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_effect = sum(rating - movie_effect - mu_hat)/(n()+lambda))
genre_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  group_by(genres) %>%
  summarise(genre_effect = sum(rating - movie_effect - user_effect - mu_hat)/(n()+lambda))
release_year_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(genre_effect, by="genres") %>%
  group_by(year) %>%
  summarise(release_year_effect = sum(rating - movie_effect - user_effect - genre_effect - mu_hat)/(n()+lambda))
review_delay_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(genre_effect, by="genres") %>%
  left_join(release_year_effect, by="year") %>%
  group_by(reviewDelay) %>%
  summarise(review_delay_effect = sum(rating - movie_effect - user_effect - genre_effect - release_year_effect - mu_hat)/(n()+lambda))

predicted.final <- final_holdout_test %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(genre_effect, by="genres") %>%
  left_join(release_year_effect, by="year") %>%
  left_join(review_delay_effect, by="reviewDelay") %>%
  mutate(pred = mu_hat + movie_effect + user_effect + genre_effect + release_year_effect + review_delay_effect) %>%
  pull(pred)

# Results on final_holdout_test
rmse.fht <- RMSE(final_holdout_test$rating, predicted.final)