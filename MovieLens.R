#Ricardo Zuñiga Gomez de la Mata
#Begging of provided code

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(data.table)
library(tidyverse)
library(dplyr)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

#set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `
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


#end of provided code

# check number of rows and columns from edx data set
ncol(edx)  #show number of columns
nrow(edx)  #show number of rows

#number of rates given in edx data set
filter(edx,rating=="3")%>% summarize(n()) #number of 3 stars
filter(edx,rating=="0")%>% summarize(n()) #number of 0 stars


#number of diferent movies given in edx data set
x<-data.frame(unique(edx$movieId))
nrow(x)

#number of diferent users given in edx data set
y<-data.frame(unique(edx$userId))
nrow(y)

#number of diferent genres given in edx data set
filter(edx,genres%like%"Drama")
filter(edx,genres%like%"Comedy")
filter(edx,genres%like%"Thriller")
filter(edx,genres%like%"Romance")

#order of most given raitings
hist(edx$rating)



#begginig of Preprocessing

edx_sep_by_genre<-edx %>% separate_rows(genres,sep="\\|")  #separate films classified with more than one genre into several observations of the same film but each one with different genre
edx_sep_by_genre$timestamp<-as_datetime(edx_sep_by_genre$timestamp) #making the timestamp variable of the edx set in a date readily format variable

#calculate the bias by movie
mu <- mean(edx$rating) 

movie_edx <- edx%>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
  b_i_mean<-mean(movie_edx$b_i) #calculate the mean bias per movie

#calculate the bias by user
user_edx <- edx %>% 
  left_join(movie_edx, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  b_u_mean<-mean(user_edx$b_u) #calculate the mean bias per user



#calculate the bias by genre
 genre_edx <- edx_sep_by_genre %>% 
  left_join(movie_edx, by='movieId') %>%
  left_join(user_edx, by='userId') %>%
  group_by(genres)%>%
  mutate(b_if=ifelse(is.na(b_i),b_i_mean,b_i))%>%   #in this experiment is not necessary just in the case the algorithm is used for new films we add as mean value
  summarize(va = mean(rating - mu - b_i-b_u))

#calculate the bias by date
date_edx<-edx_sep_by_genre %>% 
  left_join(movie_edx, by='movieId') %>%
  left_join(user_edx, by='userId') %>%
  left_join(genre_edx,by='genres')%>%
  mutate(date = round_date(timestamp, unit = "week")) %>%
  group_by(date) %>%
  summarize(b_d = mean(rating- mu - b_i- b_u- va))



#separate genres from the validation set
validation_genres<-validation%>%separate_rows(genres,sep="\\|")  #separate now the movies of the validation set per genre
validation_genres$timestamp<-as_datetime(validation_genres$timestamp) # making the timestamp variable of validation set in a date readily format variable



#calculate the RMSE

results<-data.frame() #create a data frame where results would be saved
# variable format as "results[x,1]" is the name of the observation case we are predicting
#variable format as "results[x,2]" is the RMSE result of the observation case

#case 1 just by mean

results[1,1]<-"mean"
results[1,2]<-RMSE(mu, validation_genres$rating)

#case 2 just by mean and movie bias
results[2,1]<-"movie b"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
results[2,2]<- RMSE(predicted_ratings, validation_genres$rating)

#case 3 just by mean, movie and user bias
results[3,1]<-"user b"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx, by='movieId') %>%
  left_join(user_edx, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
results[3,2]<- RMSE(predicted_ratings, validation_genres$rating)

# case 4 just by mean, movie, usear and genre bias 
results[4,1]<-"genre b"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx, by='movieId') %>%
  left_join(user_edx, by='userId') %>%
  left_join(genre_edx,by='genres') %>%
  mutate(pred = mu + b_i + b_u+va) %>%
  pull(pred)
results[4,2]<- RMSE(predicted_ratings, validation_genres$rating)  

# cae 5 whole bias (mean, movie, user, genre and date)
results[5,1]<-"date b"

predicted_ratings <- validation_genres %>% 
left_join(movie_edx, by='movieId') %>%
left_join(user_edx, by='userId') %>%
left_join(genre_edx,by='genres') %>%
  mutate(date = round_date(timestamp, unit = "week")) %>%
  left_join(date_edx,by='date')%>%
  mutate(b_if=ifelse(is.na(b_i),b_i_mean,b_i))%>%          #It is not neccesary just in the case the algorithm is used for new films we add as mean value
  mutate(b_uf=ifelse(is.na(b_u),b_u_mean,b_u))%>%          #It is not neccesary just in the case the algorithm is used for new user we add as mean value
mutate(pred = mu + b_if + b_uf+va+b_d) %>%
pull(pred)
results[5,2]<- RMSE(predicted_ratings, validation_genres$rating)

#Regularization


lambda<-seq(0,3,0.1) #vector in which we gone to try fit the best lambda

#creates a function whihc try every value of lambda to detect the value that minimize RMSE
regularization <- sapply(lambda, function(x){
  
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+x))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+x))
  
  va<- edx_sep_by_genre %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres)%>%
    summarize(va = sum(rating - b_i - mu- b_u)/(n()+x))
  
  b_d<-edx_sep_by_genre %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(va, by='genres')%>%
    mutate(date = round_date(timestamp, unit = "week")) %>%
    group_by(date) %>%
    summarize(b_d = sum(rating - b_i - mu-b_u-va)/(n()+x))
  
  predicted_ratings <- 
    edx_sep_by_genre %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(va, by = "genres") %>%
    mutate(date = round_date(timestamp, unit = "week")) %>%
    left_join(b_d, by = "date") %>%
    mutate(pred = mu + b_i + b_u+va+b_d) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_sep_by_genre$rating))
})


qplot(lambda, regularization) #plot all lambdas to the RMSE provided
reg<-lambda[which.min(regularization)]  #choose the best lambda value
reg

#Calculate the bias with regularization


  mutate(pred = mu + b_i + b_u) %>%movie_edx_reg<- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+reg))

user_edx_reg<- edx %>% 
  left_join(movie_edx_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+reg))

genre_edx_reg<- edx_sep_by_genre %>% 
  left_join(movie_edx_reg, by='movieId') %>%
  left_join(user_edx_reg, by='userId') %>%
  group_by(genres)%>%
  summarize(va = sum(rating - b_i - mu- b_u)/(n()+reg))

date_edx_reg<-edx_sep_by_genre %>% 
  left_join(movie_edx_reg, by='movieId') %>%
  left_join(user_edx_reg, by='userId') %>%
  left_join(genre_edx_reg, by='genres')%>%
  mutate(date = round_date(timestamp, unit = "week")) %>%
  group_by(date) %>%
  summarize(b_d = sum(rating - b_i - mu-b_u-va)/(n()+reg))


#case 6 just by mean and movie bias
results[6,1]<-"movie reg"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx_reg, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
results[6,2]<- RMSE(predicted_ratings, validation_genres$rating)

#case 7 just by mean, movie and user bias
results[7,1]<-"user reg"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx_reg, by='movieId') %>%
  left_join(user_edx_reg, by='userId') %>%
  pull(pred)
results[7,2]<- RMSE(predicted_ratings, validation_genres$rating)

#case 8 just by mean, movie, usear and genre bias 
results[8,1]<-"genre reg"
predicted_ratings <- validation_genres %>% 
  left_join(movie_edx_reg, by='movieId') %>%
  left_join(user_edx_reg, by='userId') %>%
  left_join(genre_edx_reg,by='genres') %>%
  mutate(pred = mu + b_i + b_u+va) %>%
  pull(pred)
results[8,2]<- RMSE(predicted_ratings, validation_genres$rating) 

#case 9 whole bias regulated
results[9,1]<- "Final"
predicted_ratings_reg <- 
  validation_genres %>% 
  left_join(movie_edx_reg, by = "movieId") %>%
  left_join(user_edx_reg, by = "userId") %>%
  left_join(genre_edx_reg, by = "genres") %>%
  mutate(date = round_date(timestamp, unit = "week")) %>%
  left_join(date_edx_reg, by = "date") %>%
  mutate(pred = mu + b_i + b_u+va+b_d) %>%
  pull(pred)

results[9,2]<- RMSE(predicted_ratings_reg, validation_genres$rating)

#input the col names of the results data frame
colnames(results)<-c("Situation","RMSE")

results<-results%>%arrange(desc(RMSE)) #sort cases down

results %>% knitr::kable() #all cases shown in a table

results<-results%>%mutate(precision=ifelse(RMSE<0.8649,"SUCCESS","FAIL")) #categorize results as success if it was under  RMSE proposed, and as fail if RMSE was higher

ggplot(results,aes(Situation,RMSE))+
geom_col(aes(fill = precision))+  
  geom_hline(yintercept = 0.8649,color='black',size = .5)+
  coord_flip()+
  scale_fill_manual(values = c("darkred","grey"))              #bar plot that show the results and fill with red the attempts that have higher RMSE that proposed

min(results$RMSE)   # return the final RMSE



