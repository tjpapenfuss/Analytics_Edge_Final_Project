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
##### Ahh HA moment. Right now we have too many factors. For example, beer_style there
##### are over 100 options. We are going to need to trim this down in order to do any meaningful
##### simulations. 
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
library(tidyverse)

# You can use the below code however I highly recommend using the full dataset.
# The full dataset is too big to load into GIT. You must store it locally!
# Import the beer reviews data set. Below I am using the truncated version.
# In the future we should update this to include all beers. 
# beer.df <- read.csv("beer_reviews_truncated.csv")

# The below code reads in all beers.
# You must put this csv file in your own directory.
# DO NOT PUT THE CSV IN THE GITHUB FOLDER. You will not be able to upload it. 
beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
count(beer.df)
# The below code will remove the NaNs.
beer.df = na.omit(beer.df)
# beer.df$beer_style.factor = as.factor(beer.df$beer_style)


summary(beer.df)
str(beer.df)

brewery_count = beer.df %>% count(brewery_name, sort = TRUE)
brewery_count #This gives the count of the beer styles

data_new <- beer.df[order(beer.df$brewery_name, increasing = TRUE), ]  # Order data descending

data_new <- Reduce(rbind,                                 # Top N highest values by group
                    by(data_new,
                       data_new["brewery_name"],
                       head,
                       n = 10))

#A question we need to ask. Do we want to split the data on the beer styles?
#Or do we split data based on the brewery? How do we want to present the data?
set.seed(123)
split = createDataPartition(beer.df$beer_style, p = 0.90, list = FALSE) 
beer.train = beer.df[split, ]
beer.test = beer.df[-split, ]
head(beer.test)
df = subset(beer.test, select = -c(review_aroma,review_appearance,
                                   review_palate,review_taste,review_time))
df = transform(df, review_overall  = as.integer(2*df$review_overall),
                   beer_abv        = as.integer(df$beer_abv*100))
# df$review_overall = as.integer(2*df$review_overall)
# df$beer_abv = as.integer(round(df$beer_abv*100, 0))
#df = df %>% 
#  mutate(abv_int_factor = case_when(
#    beer_abv < 0.5 ~ 0,
#    beer_abv < 1.5 ~ 1,
#    beer_abv < 2.5 ~ 2,
#    beer_abv < 3.5 ~ 3,
#    beer_abv < 4.5 ~ 4,
#    TRUE ~ 5))

df$abv_int_factor = as.factor(df$abv_int_factor)

df$brewery_id = as.factor(df$brewery_id)
df$beer_abv = as.factor(df$beer_abv)
str(df)
summary(df)
head(df)

regressor = lm(review_overall ~ brewery_name, data = df)
summary(regressor)

# Plotting the beer style counts. This is all beer styles
ggplot(aes(x=beer_style.factor, y = ..count..), data = beer.df) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#This gets the count of each unique beer style
style_count = beer.df %>% count(beer_style, sort = TRUE)
style_count #This gives the count of the beer styles

#This gets the number of reviews for each unique profile name
profiles_count = beer.df %>% count(review_profilename, sort = TRUE)
profiles_count #This gives the count of the profile names


#Filter beer by American IPA. This gets rid of all beers except American IPA.
beer.ipa = filter(beer.train, beer_style == "American IPA")
head(beer.apa)

# # Warning this pot is very convoluted.
# ggplot(aes(x=brewery_name, y = ..count..), data = beer.ipa) +
#   geom_bar(stat = "count") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# #Get the unique breweries that create IPAs. 
brewery_count = beer.ipa %>% count(brewery_name, sort = TRUE)
brewery_count #This gives the count of the beer styles

# ggplot(beer.ipa, aes(x = brewery_name, y = review_overall)) + geom_point() +
#   ggtitle("Age vs. Readmission") + xlab("Age") + ylab("Readmission")

# Standard linear regressor. Predicting on only IPA beers and their brewery.
# regressor = lm(review_overall ~ brewery_name, data = beer.ipa)
# summary(regressor)

# Standard linear regressor. Predicting based on all beers.
# regressor = lm(review_overall ~ ., data = beer.train)
# summary(regressor)

# Fitting the CART model
# This code takes quite some time to run. 
start_time <- Sys.time()
print("CART Starting. ")

tree <- rpart(review_overall ~ ., data = df, method="class", cp=0.001)
end_time <- Sys.time()
end_time - start_time
print("Above is the time it took to run the CART model.")
# Plotting the CART tree
pdf('readmission_tree.pdf',12,6)
prp(tree, varlen=0,faclen=0,digits=3)
dev.off()
pred <- predict(tree, newdata=readm.test, type="class")
confusion.matrix = table(readm.test$readmission, pred)
confusion.matrix
TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,])
TPR
FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,])
FPR

#cv.trees.cart = train(SalePrice ~ ., method = "rpart", data = ames.train,
#                      trControl = trainControl(method = "cv", number = 10), 
#                      tuneGrid = data.frame(.cp = seq(.00002,.002,.00002)))

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# Going to attempt to run a recommender model on the data set
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
str(beer.df)

#uniqueID <- unique(beer.df$review_profilename)
#targetID <- sapply(split(beer.df, beer.df$review_profilename), function(x) nrow(x) >= 250)

#beer_top_profiles <- beer.df[beer.df$review_profilename %in% uniqueID[targetID], , drop = FALSE]


beer_top_profiles = beer.df %>% group_by(review_profilename) %>%
  filter(n() >= 100)
beer_top_beers = beer.df %>% group_by(beer_beerid) %>%
  filter(n() >= 50)
str(beer_top_profiles)
uniqueID.profiles <- unique(beer_top_profiles$review_profilename)
uniqueID.beers <- unique(beer_top_beers$beer_beerid)

beer_top_profiles <- beer.df[beer.df$review_profilename %in% uniqueID.profiles[], , drop = FALSE]
beer_final <- beer_top_profiles[beer_top_profiles$beer_beerid %in% uniqueID.beers[], , drop = FALSE]

beer_top_profiles %>%
  group_by(review_overall) %>%
  summarize(cases = n()) %>%
  ggplot(aes(review_overall, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:5) 

#Remove the 0 and 0.5 review outliers. 
beer_final = beer_final[beer_final$review_overall!= 0, ]
beer_final = beer_final[beer_final$review_overall!= 0.5, ]



# First I need to get a subset of the data. This subset will have the username, beer name, and rankings 
beer.sub = beer_final[c("review_profilename", "beer_beerid", "review_overall")]
beer.sub$review_id= as.numeric(factor(beer.sub$review_profilename))
# Display with dependent variable first

beer_sub_recommender = beer.sub[c("review_id", "beer_beerid", "review_overall")]
beer_sub_recommender$review_overall = as.numeric(beer_sub_recommender$review_overall)
#beer_sub_recommender$review_id = as.numeric(beer_sub_recommender$review_id)
#beer_sub_recommender = beer_sub_recommender[order(beer_sub_recommender$review_id),]

beer_sub_recommender = beer_sub_recommender %>% distinct(review_id, beer_beerid, .keep_all = TRUE)

set.seed(144)
train.idx <- sample(seq_len(nrow(beer_sub_recommender)), 0.99*nrow(beer_sub_recommender))
beer.train <- beer_sub_recommender[train.idx,]
beer.test <- beer_sub_recommender[-train.idx,]

#set.seed(123)
#split = createDataPartition(beer_sub_recommender$review_id, p = 0.99, list = FALSE) 
#beer.train = beer_sub_recommender[split, ]
#beer.test = beer_sub_recommender[-split, ]
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
str(train.1m)
str(beer.train)
recompute <- TRUE

if (recompute) {
  set.seed(144)
  cv.all.1m.beer <- CV.recommender(beer.train, 2, 2:4)
  
} else {
  load("cv_all_1m.RData")
}
cv.info.1m.beer <- cv.all.1m.beer$info
cv.pred.1m.beer <- cv.all.1m.beer$pred
cv.pred9.beer <- cv.pred.1m.beer[[2]]  # Cross-validation predictions with rank 9

### Plotting the cross-validation output
cv.all.1m.beer$info$SEETP

print(ggplot(cv.info.1m.beer, aes(x=rank, y=r2)) +
        geom_point(size=3) +
        theme_bw() +
        xlab("Number of archetypes (k)") +
        ylab("Cross-Validation R2") +
        theme(axis.title=element_text(size=18), axis.text=element_text(size=18)))


### Final model, using results from cross-validation

mat.final <- Incomplete(train.1m[, 1], train.1m[, 2], train.1m[, 3])

set.seed(144)
fit <- softImpute(mat.final, rank.max=9, lambda=0, maxit=1000)




# -----
fold <- sample(seq_len(2), nrow(movie.1m), replace = TRUE)
pred <-
  replicate(2, rep(NA, nrow(movie.1m)), simplify = FALSE)
minimum <- min(movie.1m[, 3])
maximum <- max(movie.1m[, 3])
for (f in seq_len(folds)) {
  print(f)
  mat <- Incomplete(movie.1m[fold != 1, 1], movie.1m[fold != 1, 2],
                    movie.1m[fold != 1, 3])
  for (r in seq_along(ranks)) {
    fit <- softImpute(mat,
                      rank.max = 3,
                      lambda = 0,
                      maxit = 1000)
    # fit$u returns the weights for all user-archetype pairs
    # fit$v returns an indicator of the ratings for all archetype-movie pairs
    
    pred[[r]][fold == f] <-
      pmin(pmax(impute(fit, dat[fold == f, 1], dat[fold == f, 2]), minimum), maximum)
  }
}
# baseline: predict the average movie rating for all user-movie pairs
base.fold <-
  sapply(seq_len(folds), function(f)
    mean(dat[fold != f, 3]))
