## Alexander Robertson
## 12/27/2020
## Playing around with the data I generated over the last year watching movies.
## For more info, visit www.aj-robertson.com

## Ideas to expand on in the future
    ## Time of year vs rating given
        ## Scatterplot of data by time of year
    ## Corr plot to find which factors are most likely to influence higher ratings

## Imported as ""movies_master"
## Create a backup
m=movies_master

head(m)
dim(m)

## Cleaning to just what I want
columns = c("title","year","director1","genre1","genre2","personal_rating", "rewatch", "watchdate", "mpa")
m = m[,columns]


####### Looking at my overall rating distribution  ##########
## Creating a column with rounded rating
m$roundrating = round(m$personal_rating, 0)
rat_dist = as.data.frame(table(m$roundrating))
names(rat_dist)[1] = "rating"
names(rat_dist)[2] = "freq"
grat_dist = ggplot(data = rat_dist, aes(x =rating, y = freq, fill=rating))
grat_dist + geom_col() + labs(title="Distribution of ratings across all movies", x = "Personal Rating", y = "Number of movies") + theme(legend.position = "none")


############## Analysis of by movie release date #########################
## Plotting personal rating by year movie was released 
library(ggplot2)
g = ggplot(data = m, aes(x=year, y=personal_rating, color=genre1))
g + geom_point() + labs(title="Personal Rating by Year of Movie Release", y = "Personal Rating (1-10)", x="Year of movie release", color = "Primary genre")

## Making a decades column
m$decade = m$year - m$year %% 10

## The number of movies watched by decade
dec = as.data.frame(table(m$decade))
names(dec)[1] = "dec"
names(dec)[2] = "freq"
gdec = ggplot(data = dec, aes(x = dec, y = freq, fill=dec))
gdec + geom_col() + labs(title="Movies watched in 2020 by decade", x = "Decade", y = "Total number") + theme(legend.position = "none")

## Finding the average rating by decade. Then seeing if there's a statistically significant difference
library(doBy)
avgdec = summaryBy(personal_rating~decade, data = m, FUN = mean, na.rm = TRUE)
names(avgdec)[1] = "decade"
names(avgdec)[2] = "avg_rating"

gavgdec = ggplot(data = avgdec, aes(x=decade, y=avg_rating, fill = decade))
gavgdec + geom_col() + labs(title="Average rating by decade", y = "Average Personal Rating (1-10)", x="Decade of movie release") + theme(legend.position = "none")

## ANOVA to see differences
summary(aov(personal_rating~decade, data = m))





################## Looking at genre ####################
## Finding the number of movies by genre1 
gen = as.data.frame(table(m$genre1))
names(gen)[1] = "genre"
names(gen)[2] = "freq"
## Reordering it from largest to smallest 
gen = transform(gen, genre = reorder(genre, -freq))
ggen = ggplot(data = gen, aes(x=genre, y=freq, fill = genre))
ggen + geom_col() + labs(title="Movies watched in 2020 by primary genre", x = "Genre", y = "Total number") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

## Finding the average ratings by genre and then finding if there is a statistical significance between them
## Removing any genres with less than 3 entries
library(dplyr)
small_gen = m %>%
  group_by(genre1) %>%
  filter(n() > 3)
## Creating a table of average scores and transforming it from largest to smallest
avgg = summaryBy(personal_rating~genre1, data = small_gen, FUN = mean, na.rm = TRUE)
avgg = transform(avgg, genre1 = reorder(genre1, -personal_rating.mean))
## Graphing table
gavgg = ggplot(data = avgg, aes(x=genre1, y = personal_rating.mean, fill = genre1))
gavgg + geom_col() + labs(title="Average ratings by primary genre", x = "Genre", y = "Average Rating (1-10)") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

## Significant difference between scores?
summary(aov(personal_rating~genre1, data=m))





############ Looking at MPA ratings ################
## Playing around with MPA ratings
rat = as.data.frame(table(m$mpa))
names(rat)[1] = "mpa"
names(rat)[2] = "freq"
rat
## Graphing rat
rat = transform(rat, mpa = reorder(mpa, -freq))
grat = ggplot(data = rat, aes(x=mpa, y=freq, fill = mpa))
grat + geom_col() + labs(title="Movies watched in 2020 by MPA Rating", x = "Rating", y = "Total number") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")



