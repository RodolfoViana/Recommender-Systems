# Assignment 1 Instructions: Non-Personalized Recommenders

# Mean Rating: Calculate the mean rating for each movie, order with the highest rating listed first, and submit the top 5.

meanRating <- function (){
  outcome <- read.csv("/home/rodolfo/Coursera/Recommender Systems/A1Ratings.csv")
  movies <- outcome[, 2:20]
  
  meanMovies <- vapply(movies, mean, 2,na.rm = TRUE)
  sort(meanMovies, TRUE)[1:5]
  
  # Anws 318, 260, 541, 1265, 593 
}


# Calculate the percentage of ratings for each movie that are 4 or higher.

ratingHigher <- function (vector){
  
  vector <- vector[!is.na(vector)]
  nRatings <- length(vector)
  vector <- vector[vector >= 4]
  nRatingsHigher <- length(vector)
  
  nRatingsHigher/nRatings
}


# Order with the highest percentage first, and submit the top 5.

rating4 <- function (){
  outcome <- read.csv("/home/rodolfo/Coursera/Recommender Systems/A1Ratings.csv")
  movies <- outcome[, 2:20]
  
  ratingMovies <- vapply(movies, ratingHigher, 2)
  sort(ratingMovies, TRUE)[1:5]
  
  #Anws 318 260 3578 541 593
}


# Count how many not na value has in the vector

countWithoutNA <- function(vector){
  length(vector[!is.na(vector)])
}


# Rating Count: Count the number of ratings for each movie, order with the most number of ratings first, 
# and submit the top 5.

ratingCount <- function(){
  outcome <- read.csv("/home/rodolfo/Coursera/Recommender Systems/A1Ratings.csv")
  movies <- outcome[, 2:20]
  
  counting <- vapply(movies, countWithoutNA, 2)
  sort(counting, TRUE)[1:5]
  
  # Anws 1, 593, 260, 1210, 780
}


# Top 5 Star Wars: Calculate movies that most often occur with 
# Star Wars: Episode IV - A New Hope (1977) using the (x+y)/x method described in class. 
# In other words, for each movie, calculate the percentage of Star Wars raters who also rated that movie. 
# Order with the highest percentage first, and submit the top 5.

starWarsAssociation <- function(){
  outcome <- read.csv("/home/rodolfo/Coursera/Recommender Systems/A1Ratings.csv")
  starWars <- outcome[, 2]
  movies <- outcome[, 3:20]
  
  association <- vapply(movies, associationGeneric, 2, starWars)
  sort(association, TRUE)[1:5]
  
  #Anws 1, 1210, 593, 780, 2916
  
}


# Calculate the association between two vectors

associationGeneric <- function(vectorA, vectorB){
  vectorA <- !is.na(vectorA)
  vectorB <- !is.na(vectorB)
  
  awns <- vectorA & vectorB
  sum(awns)/sum(vectorB)
}
