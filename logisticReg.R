library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(softImpute)
library(ggplot2)
library(stringr)
library(tidyverse)



# Import the data set.
beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
str(beer.df)

# Get the top profiles. 
beer_top_profiles = beer.df %>% group_by(review_profilename) %>%
  filter(n() >= 100)
# Get the top beers by number of ratings. 
beer_top_beers = beer.df %>% group_by(beer_beerid) %>%
  filter(n() >= 50)
str(beer_top_profiles)

# Get the unique counts of the profiles and beers. 
uniqueID.profiles <- unique(beer_top_profiles$review_profilename)
uniqueID.beers <- unique(beer_top_beers$beer_beerid)
# Remove all the profiles and beers that are small in number. 
beer_top_profiles <- beer.df[beer.df$review_profilename %in% uniqueID.profiles[], , drop = FALSE]
beer_final <- beer_top_profiles[beer_top_profiles$beer_beerid %in% uniqueID.beers[], , drop = FALSE]

# GGPLOT to see the average ratings of beers.  
beer_top_profiles %>%
  group_by(review_overall) %>%
  summarize(cases = n()) %>%
  ggplot(aes(review_overall, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:5) 

#Remove the 0 and 0.5 review outliers. 
beer_final = beer_final[beer_final$review_overall!= 0, ]
beer_final = beer_final[beer_final$review_overall!= 0.5, ]

# Now we get a subset of the data. This subset will have the username, beer name, and rankings 
beer.sub = beer_final[c("review_profilename", "beer_beerid", "review_overall")]
# Create a "user id" for the reviewers. This is necessary for the algo.
beer.sub$review_id = as.integer(factor(beer.sub$review_profilename))
# Display with dependent variable first

# Get the final subset. This is the data that will be fed into the recommender. 
beer_sub_recommender = beer.sub[c("review_id", "beer_beerid", "review_overall")]
beer_sub_recommender$review_overall = as.numeric(beer_sub_recommender$review_overall)
#beer_sub_recommender$review_id = as.numeric(beer_sub_recommender$review_id)
#beer_sub_recommender = beer_sub_recommender[order(beer_sub_recommender$review_id),]

# Remove duplicate data. 
beer_sub_recommender = beer_sub_recommender %>% distinct(review_id, beer_beerid, .keep_all = TRUE)

# Split the data set. 
set.seed(144)
train.idx <- sample(seq_len(nrow(beer_sub_recommender)), 0.99*nrow(beer_sub_recommender))
beer.train <- beer_sub_recommender[train.idx,]
beer.test <- beer_sub_recommender[-train.idx,]