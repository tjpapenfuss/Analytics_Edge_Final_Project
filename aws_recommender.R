# ----------------------------------------------------------------------------------------
##### Project outline
# ----------------------------------------------------------------------------------------
##### Project Initialization
##### Data can be found here: https://www.kaggle.com/datasets/rdoume/beerreviews
##### Download the CSV file and use that as your beef.df below. 
# ----------------------------------------------------------------------------------------
##### Some sample ideas for testing. 
##### Predicting review score via CART
##### Predicting beer score for a brewery releasing a new beer via logistic regression
##### 
# ----------------------------------------------------------------------------------------
##### install.packages("package_name")
# ----------------------------------------------------------------------------------------

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
# library(tidyverse)
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

# I commented these out. The 1 - 2.5 reviews are very few. 
# This is causing us issues. 
# beer_final = beer_final[beer_final$review_overall!= 1, ]
# beer_final = beer_final[beer_final$review_overall!= 1.5, ]

# beer_final = beer_final[beer_final$review_overall!= 2, ]
# beer_final = beer_final[beer_final$review_overall!= 2.5, ]

# Now we get a subset of the data. This subset will have the username, beer name, and rankings 
beer.sub = beer_final[c("review_profilename", "beer_beerid", "review_overall")]
# Create a "user id" for the reviewers. This is necessary for the algo.
beer.sub$review_id = as.numeric(factor(beer.sub$review_profilename))
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

head(beer.test)
### The following function performs collaborative filtering and cross-validation to find the appropriate rank (k)
# dat: data frame of ratings
# folds: number of folds for k-fold cross-validation
# ranks: all the ranks to be tested
# dat should have the following form:
# first column: user id
# second column: movie id
# third column: ratings
CV.recommender <- function(dat, folds, ranks) {
  fold <- sample(seq_len(folds), nrow(dat), replace = TRUE)
  pred <-
    replicate(length(ranks), rep(NA, nrow(dat)), simplify = FALSE)
  minimum <- min(dat[, 3])
  maximum <- max(dat[, 3])
  for (f in seq_len(folds)) {
    print(f)
    mat <- Incomplete(dat[fold != f, 1], dat[fold != f, 2],
                      dat[fold != f, 3])
    for (r in seq_along(ranks)) {
      fit <- softImpute(mat,
                        rank.max = ranks[r],
                        lambda = 0,
                        maxit = 1000)
      # fit$u returns the weights for all user-archetype pairs
      # fit$v returns an indicator of the ratings for all archetype-movie pairs
      
      pred[[r]][fold == f] <-
        pmin(pmax(impute(fit, dat[fold == f, 1], dat[fold == f, 2]), minimum), maximum)
      # print(pred[[r]][fold == f])
    }
  }
  # baseline: predict the average movie rating for all user-movie pairs
  base.fold <-
    sapply(seq_len(folds), function(f)
      mean(dat[fold != f, 3]))
  print("Here are the values: ")
  print(dat[,3])
  print("Base fold")
  print(base.fold[f])
  print("pred:")
  print(pred)
  list(
    info = data.frame(
      rank = ranks,
      r2 = sapply(pred, function(x)
        1 - sum((x - dat[, 3]) ^ 2) / sum((base.fold[f] - dat[, 3]) ^ 2)),
      SSE = sapply(pred, function(x)
        sum((x - dat[, 3]) ^ 2)),
      SEETP = sapply(pred, function(x)
        sum((base.fold[f] - dat[, 3]) ^ 2)),
      RMSE = sapply(pred, function(x)
        sqrt(mean((
          x - dat[, 3]
        ) ^ 2))),
      MAE = sapply(pred, function(x)
        mean(abs(x - dat[, 3])))
    ),
    pred = pred
  )
}

### Calling the function with our dataset
# The "recompute" test is because cross-validation is very intensive computationally.
# Therefore, we save the cross-validation output and, if we are happy with that, we just read it.
# str(train.1m)
str(beer.train)
recompute <- TRUE

if (recompute) {
  set.seed(144)
  cv.all.1m.beer <- CV.recommender(beer.train, 10, 1:20)
  
} else {
  load("cv_all_1m.RData")
}
cv.info.1m.beer <- cv.all.1m.beer$info
cv.pred.1m.beer <- cv.all.1m.beer$pred
cv.pred9.beer <- cv.pred.1m.beer[[2]]  # Cross-validation predictions with rank 9

### Plotting the cross-validation output

print("SSETP:")
cv.all.1m.beer$info$SEETP
print("SSE:")
cv.all.1m.beer$info$SSE


ggplot(cv.info.1m.beer, aes(x=rank, y=r2)) +
  geom_point(size=3) +
  theme_bw() +
  xlab("Number of archetypes (k)") +
  ylab("Cross-Validation R2") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


### Final model, using results from cross-validation

# mat.final <- Incomplete(beer.train[, 1], beer.train[, 2], beer.train[, 3])

#set.seed(144)
#fit <- softImpute(mat.final, rank.max=9, lambda=0, maxit=1000)