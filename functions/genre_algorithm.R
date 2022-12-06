
# ============================================================
# Functions used in implementation of collaborative filtering.
# ============================================================

library(Matrix)
library(recommenderlab)
library(slam)
library(data.table)
library(tidyverse)

#define functions to get ratings from online data
readin_data = function() {
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  ratings$Timestamp = NULL
  return(ratings)
}
readings = readin_data()

#------------Movie Data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

predict_genre_helper = function(ratings, movies, user_genre) {
  n_display = 10
  #ratings = readin_data();
  movies.genre = movies %>%
    filter(grepl(user_genre,movies$Genres))
  tmp.12 = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig = 3)) %>%
    inner_join(movies.genre, by = 'MovieID') %>%    
    filter(ratings_per_movie > 1000) %>%
    top_n(n_display, ratings_per_movie) %>%
    arrange(desc = -1 * ave_ratings)
  return(tmp.12)
}

genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")

l_tmp = hash()
for (genre.item in genre_list) {
  l_tmp[[genre.item]] = predict_genre_helper(readings, movies, genre.item)
}

#
predict_genre = function(user_genre) {
  return(l_tmp[[user_genre]])
}
