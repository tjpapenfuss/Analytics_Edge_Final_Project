library(tm)
library(SnowballC)
library(caret)
library(rpart)
library(rpart.plot)
library(softImpute)
library(dplyr)

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# First part here is setting up the data. 
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# Import the data set.
beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
str(beer.df)
beer.df = na.omit(beer.df)

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

# I commented these out. The 1 - 2.5 reviews are very few. 
# This is causing us issues. 
# beer_final = beer_final[beer_final$review_overall!= 1, ]
# beer_final = beer_final[beer_final$review_overall!= 1.5, ]

# beer_final = beer_final[beer_final$review_overall!= 2, ]
# beer_final = beer_final[beer_final$review_overall!= 2.5, ]

# Now we get a subset of the data. This subset will have the username, beer name, and rankings 
beer.sub = beer_final[c("review_profilename", "beer_beerid", "review_overall")]
# Create a "user id" for the reviewers. This is necessary for the algo.
beer.sub$review_id = as.integer(factor(beer.sub$review_profilename))
# beer.sub$review_id = as.numeric(factor(beer.sub$review_profilename))
# Display with dependent variable first

# Get the final subset. This is the data that will be fed into the recommender. 
beer_sub_recommender = beer.sub[c("review_id", "beer_beerid", "review_overall")]
beer_sub_recommender$review_overall = as.numeric(beer_sub_recommender$review_overall)
#beer_sub_recommender$review_id = as.numeric(beer_sub_recommender$review_id)
#beer_sub_recommender = beer_sub_recommender[order(beer_sub_recommender$review_id),]

# Remove duplicate data. 
beer_sub_recommender = beer_sub_recommender %>% distinct(review_id, beer_beerid, .keep_all = TRUE)

# Split the data set. 
# set.seed(144)
#train.idx <- sample(seq_len(nrow(beer_sub_recommender)), 0.99*nrow(beer_sub_recommender))
#beer.train <- beer_sub_recommender[train.idx,]
#beer.test <- beer_sub_recommender[-train.idx,]

#head(beer.test)

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# Going to attempt to run a recommender model on the data set
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# reading the function file
source("functionsCF.R")

# splitting into a training set and a test set
set.seed(144)
training.rows <- cf.training.set(beer_sub_recommender$review_id, beer_sub_recommender$beer_beerid, prop=0.92)
beer.train <- beer_sub_recommender[training.rows,]
beer.test <- beer_sub_recommender[-training.rows,]
mat.train <- Incomplete(beer.train[,1], beer.train[,2], beer.train[,3])
?Incomplete
# scaling the matrix
set.seed(15071)
mat.scaled <- biScale(mat.train, maxit=1000, row.scale = FALSE, col.scale = FALSE)

### Question a.
beer.train = na.omit(beer.train)
str(music.train)
str(beer.train)
summary(music.train)
summary(beer.train)

set.seed(123)
out_eval = cf.evaluate.ranks(beer.train, 1:2, prop.validate=0.05)
ggplot(data = out_eval) +
  geom_point(aes(rank, r2))


### Question b.

set.seed(15071)
# We are now ready to apply our collaborative filtering algorithm.
# This is done with the softImpute function, as follows:
fit <- softImpute(mat.train, rank.max=4, lambda=0, maxit=1000)


pred.insample <- impute(fit, music.train[, 1], music.train[, 2])
pred.outsample <- impute(fit, music.test[, 1], music.test[, 2])

R2.insample <- 1 - sum((pred.insample-music.train$rating)^2)/sum((mean(music.train$rating) - music.train$rating)^2)
R2.outsample <- 1 - sum((pred.outsample-music.test$rating)^2)/sum((mean(music.train$rating) - music.test$rating)^2)
R2.insample
R2.outsample
?impute
outTable = fit$u
Daisy = (fit$u)[1584,]

hist(pred.insample)
hist(pred.outsample)

# complete(mat.train,fit)
# impute(music.train,fit)
### Question c.

sum(fit$u[music.train[1584,1],] * fit$v[131,] * fit$d)
sum(fit$u[music.train[1584,1],] * fit$v[156,] * fit$d)

### Question d.
daisy_rated = music[which(music$userID ==1584),2]
daisy_full = music[which(music$userID ==1584),]

daisy_df = data.frame("songID"=c(0), "Estimated_Rating"=c(0))
for (x in 1:807) {
  if (!(x %in% daisy_rated)) {
    pred.Daisy = sum(fit$u[music.train[1584,1],] * fit$v[x,] * fit$d)
    daisy_df[x,] = c(x,pred.Daisy)    
  }
}
sort(daisy_df$Estimated_Rating)
top_n(daisy_df,5)

sort(daisy_rated)
top_n(daisy_full,5)

daisy_final_rated = merge(top_n(daisy_full,5), songs, by=c("songID"))
daisy_final_recommend = merge(top_n(daisy_df,5), songs, by=c("songID"))
write.csv(daisy_final_rated, "finalrated.csv")
write.csv(daisy_final_recommend, "daisyRecommended.csv")