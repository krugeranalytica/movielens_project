## During the conversion to Rmd certain sections were re-organized to have all
## the data cleaning in one section before building the models. Also removed the
## many temporary data sets that are created in the R code for the report.

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


#############################
#project calculations
#Look at edx dataset = train_set
#############################

head(edx)

edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_total = n_users*n_movies)

#Distribution of movies getting rated more often than others
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Movies")

#Some users are more active than others
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Users")

#Use RMSE as a first indication of accuracy
# Loss function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#First Model - all ratings = mean rating
# Y(u,i) = mu + eps(u,i)
mu_hat <- mean(edx$rating)
mu_hat

#RMSE with all ratings = mu
naive_rmse <-  RMSE(validation$rating, mu_hat)
naive_rmse

#RMSE of 1 is definitely not good enough, however log this result in results df
rmse_results <- data_frame(method = "Mean only", RMSE = naive_rmse)

#Add movie effect - some movies are rated higher
# Y(u,i) = mu + b(i) + eps(u,i)
#fit <- lm(rating ~ as.factor(userId), data = edx)  -creates memory issue
# get around this by using average of Y(u,i)-mu_hat for each movie i.
mu <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#look at variation in these estimates
movie_avgs %>% qplot(b_i, geom = "histogram", bins = 10, data = ., color = I("black"))

#mu=3.5 and b_i=1.5 => rating = 3.5 + 1.5 = 5
#see if prediction improves when using y(u,i) = mu + b_i
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model", RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#user effect
#Y(u,i) = mu + b(i) + b(u) + eps(u,i)
#look at variability of average ratings from users with more than 100 ratings
edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")
#fit <- lm(rating ~ as.factor(movieId)+as.factor(userId), data = edx)  -creates memory issue
# get around this by using average of Y(u,i)-mu_hat-b(i) for each user u.
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#how much did it improve now
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

# 25 points: RMSE <= 0.87750
# Can we do better? Date effect and Genre effect

#Date -> Movie year from name
edx_p <- edx %>%
  mutate(title = str_trim(title)) %>%
  #extract year out of title
  extract(title, c("title_tmp","year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-",simplify = T)[1]), as.integer(year))) %>%
  #replace title na with orig title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  #drop title_tmp
  select(-title_tmp) 
#%>%
  #change no genres listed to na
  #mutate(genres = if_else(genres=="(no genres listed)", `is.na<-`(genres), genres))

validation_p <- validation %>%
  mutate(title = str_trim(title)) %>%
  #extract year out of title
  extract(title, c("title_tmp","year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-",simplify = T)[1]), as.integer(year))) %>%
  #replace title na with orig title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  #drop title_tmp
  select(-title_tmp) 
#%>%
  #change no genres listed to na
  #mutate(genres = if_else(genres=="(no genres listed)", `is.na<-`(genres), genres))


#year effect
#Y(y,u,i) = mu + b(i) + b(u) + b(y) + eps(u,i)
#look at variability of average ratings per year
edx_p %>%
  group_by(year) %>%
  summarize(b_y = mean(rating)) %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 30, color = "black")

#Y(y,u,i)-mu-b(i)-b(u)
year_avgs <- edx_p %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))
#how much did it improve now
predicted_ratings <- validation_p %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred
  
model_3_rmse <- RMSE(predicted_ratings, validation_p$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effects Model",
                                     RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

#Now tackle genre effect
edx_g <- edx_p %>% separate_rows(genres, sep = "\\|") %>%
  spread(genres,genres) %>%
  # fix (no genres listed)
  select(-`(no genres listed)`)
  #fix genre names with hyphens or (no genre listed)

colnames(edx_g)[c(16,22)] <- c("FilmNoir","SciFi") 


#replace genre na values with 0
edx_g <- edx_g %>%  replace_na(list(NoGenre=0,Action=0,Adventure=0,Animation=0,
                  Children=0,Comedy=0,Crime=0,Documentary=0,Drama=0,
                  Fantasy=0,FilmNoir=0,Horror=0,IMAX=0,Musical=0,Mystery=0,
                  Romance=0,SciFi=0,Thriller=0,War=0,Western=0)) 


#replace genre names in genre columns with 1
edx_g[edx_g=="(no genres listed)"|edx_g=="Action"|edx_g=="Adventure"|edx_g=="Animation"|
               edx_g=="Children"|edx_g=="Comedy"|edx_g=="Crime"|edx_g=="Documentary"|
               edx_g=="Drama"|edx_g=="Fantasy"|edx_g=="Film-Noir"|
               edx_g=="Horror"|edx_g=="IMAX"|edx_g=="Musical"|
               edx_g=="Mystery"|edx_g=="Romance"|edx_g=="Sci-Fi"|
               edx_g=="Thriller"|edx_g=="War"|edx_g=="Western"]<-1

#set up genre columns in validation set
validation_g <- validation_p %>% separate_rows(genres, sep = "\\|") %>%
  spread(genres,genres)
#fix genre names with hyphens or (no genre listed)
colnames(validation_g)[c(16,22)] <- c("FilmNoir","SciFi") 


#replace genre na values with 0
validation_g <- validation_g %>%  replace_na(list(NoGenre=0,Action=0,Adventure=0,Animation=0,
                                    Children=0,Comedy=0,Crime=0,Documentary=0,Drama=0,
                                    Fantasy=0,FilmNoir=0,Horror=0,IMAX=0,Musical=0,Mystery=0,
                                    Romance=0,SciFi=0,Thriller=0,War=0,Western=0)) 


#replace genre names in genre columns with 1
validation_g[validation_g=="(no genres listed)"|validation_g=="Action"|validation_g=="Adventure"|validation_g=="Animation"|
        validation_g=="Children"|validation_g=="Comedy"|validation_g=="Crime"|validation_g=="Documentary"|
        validation_g=="Drama"|validation_g=="Fantasy"|validation_g=="Film-Noir"|
        validation_g=="Horror"|validation_g=="IMAX"|validation_g=="Musical"|
        validation_g=="Mystery"|validation_g=="Romance"|validation_g=="Sci-Fi"|
        validation_g=="Thriller"|validation_g=="War"|validation_g=="Western"]<-1


#Genre effect
#year effect
#Y(y,u,i) = mu + b(i) + b(u) + b(y) + b(g) + eps(u,i)
#look at variability of average ratings per genre
#genre=Action
genres <- c("Action","Adventure","Animation","Children",
            "Comedy","Crime","Documentary","Drama",
            "Fantasy","FilmNoir","Horror","IMAX","Musical",
            "Mystery","Romance","SciFi","Thriller","War","Western")
            
#Calculate the average rating per genre. Apply the average for each genre that a movie belongs to.

genre_avgs <- sapply(genres, function(gnre){
  
  edx_g[edx_g[[gnre]]==1,] %>%
    left_join(user_avgs, by='userId') %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(year_avgs, by='year') %>%
    group_by(by=gnre) %>%
    summarize(b_g = mean(rating - mu - b_i - b_u - b_y))
  
})

genre_avgs

#edx_g[edx_g$Action=="1",] %>%
#  summarize(b_g = mean(rating))

#ga_avgs <- edx_g[edx_g$Action=="1",] %>%
#  left_join(user_avgs, by='userId') %>%
#  left_join(movie_avgs, by='movieId') %>%
#  left_join(year_avgs, by='year') %>%
#  group_by(Action) %>%
#  summarize(b_g_a = mean(rating - mu - b_i - b_u - b_y)) %>%
#  rbind(c(0,0))

#Test applying genre average to movie
genre_avgs[,"Action"]$b_g*
  as.numeric(edx_g[1:10,"Action"])

#Create genre_temp to with column g to save total of each genre effect for every movie
genre_temp <- edx_g %>% mutate(movieId, g=0)
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Action"]])*genre_avgs[,"Action"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Adventure"]])*genre_avgs[,"Adventure"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Animation"]])*genre_avgs[,"Animation"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Children"]])*genre_avgs[,"Children"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Comedy"]])*genre_avgs[,"Comedy"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Crime"]])*genre_avgs[,"Crime"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Documentary"]])*genre_avgs[,"Documentary"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Drama"]])*genre_avgs[,"Drama"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Fantasy"]])*genre_avgs[,"Fantasy"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["FilmNoir"]])*genre_avgs[,"FilmNoir"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Horror"]])*genre_avgs[,"Horror"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["IMAX"]])*genre_avgs[,"IMAX"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Musical"]])*genre_avgs[,"Musical"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Mystery"]])*genre_avgs[,"Mystery"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Romance"]])*genre_avgs[,"Romance"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["SciFi"]])*genre_avgs[,"SciFi"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Thriller"]])*genre_avgs[,"Thriller"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["War"]])*genre_avgs[,"War"]$b_g
genre_temp$g <- genre_temp$g + as.numeric(genre_temp[["Western"]])*genre_avgs[,"Western"]$b_g

#get average genre effect per movie id
ga_avgs <- genre_temp %>%
  group_by(movieId) %>%
  select(movieId,g) %>%
  summarize(b_g = mean(g))

#rm(genre_temp)

#how much did it improve now with genre effects
predicted_ratings <- validation_g %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year') %>%
  left_join(ga_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings, validation_g$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effects Model + Genre",
                                     RMSE = model_5_rmse))
rmse_results %>% knitr::kable()

