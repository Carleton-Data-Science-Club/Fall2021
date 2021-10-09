library(tidyverse) #import library
library(ggplot2)
library(dplyr)


imdb<-read.csv("imdb.csv") # import data
mean(imdb$rating) #find mean , missing data


imdb[1,1] # number in row 1 column 1
imdb[3,c(4,5)] # number in the row  3 column 4 and 5

## delete all the missing data
is.na(imdb$rating)
which(is.na(imdb$rating))

imdb2<-imdb[-which(is.na(imdb$rating)), ] # delete all NA in rating column

mean(imdb2$rating) # mean, alternatively you can do mean(imdb2$rating, na.rm = TRUE)


## Visualize the data
hist(imdb2$rating) # make a histogram using base R functions
hist(imdb2$year)

#find the mean rating for each kind of movie/show
imdb2 %>% group_by(kind) %>% summarise(mean_rating = mean(rating))
# find the number of rows for each kind of movie/show
num_movies <- imdb2 %>% group_by(year) %>% summarise(freq = n())

# plot a line graph of the number movies/shows produced in each year
ggplot(data = num_movies, aes(x = year, y = freq)) +
  geom_point() +
  geom_line()

# plot a scatter plot
ggplot(data = imdb2, aes(x = rating, y = vote)) +
  geom_point(aes(color = kind)) +
  geom_smooth(method = "lm") + # add a linear regression line
  scale_y_log10()


## Fitting a model
a <- lm(data = imdb, rating ~ log(vote)) # fit a regression line onto the data
summary(a) # summarzse the results
