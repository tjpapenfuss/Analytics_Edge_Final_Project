library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(xgboost)
library(gdata)
library(stringr)
library(ROCR)

# The below code reads in all beers.
beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
count(beer.df)
# The below code will remove the NaNs.
beer.df = na.omit(beer.df)

#We will take a look at beers which contain 'American' in the name
df.filter = beer.df %>% filter(grepl("American", beer_style))
#Now we will take a look at the count of breweris in this new data frame
brewery_count = df.filter %>% count(brewery_name, sort = TRUE)
brewery_count #This gives the count of the beer styles


##Next we are going to filter pm the breweries which are not in the Top N by count
##There could have been a better way to do this but I didn't have time to mess
##around or figure it out.
df.filter2 = filter(df.filter, brewery_name %in% c("Sierra Nevada Brewing Co.",
                                                   "Stone Brewing Co.",
                                                   "Dogfish Head Brewery",
                                                   "Bell's Brewery, Inc.",
                                                   "Lagunitas Brewing Company",
                                                   "Rogue Ales", 
                                                   "Founders Brewing Company",
                                                   "Southern Tier Brewing Company",
                                                   "Boston Beer Company (Samuel Adams)", 
                                                   "Deschutes Brewery" 
))


#A question we need to ask. Do we want to split the data on the beer styles?
#Or do we split data based on the brewery? How do we want to present the data?
set.seed(123)
split = createDataPartition(df.filter2$review_overall, p = 0.70, list = FALSE) 
beer.train = df.filter2[split, ]
beer.test = df.filter2[-split, ]

#Subset the data frame to only use columns we are interested in using as predictors
df.train = subset(beer.train, select = -c(review_time,review_taste, review_palate,
                                    beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))

df.test = subset(beer.test, select = -c(review_time,review_taste, review_palate,
                                    beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))
#binary.df = df
#binary.df$review_overall = if_else(binary.df$review_overall>4, 1, 0)
#str(binary.df)

#Set the categorical variables -->Check the resulting class of variables
#df$beer_style = as.factor(df$beer_style)
#df$brewery_name = as.factor(df$brewery_name)
str(df.train)

df.train$Recommended = ifelse(df.train$review_overall>= 4, 1, 0)
df.train$Recommended.factor = as.factor(df.train$Recommended)
df.train$brewery_name = as.factor(df.train$brewery_name)
df.train$beer_style = as.factor(df.train$beer_style)

df.test$Recommended = ifelse(df.test$review_overall>= 4, 1, 0)
df.test$Recommended.factor = as.factor(df.test$Recommended)
df.test$brewery_name = as.factor(df.test$brewery_name)
df.test$beer_style = as.factor(df.test$beer_style)

## xgboost
xgb <- xgboost(data = model.matrix(Recommended~brewery_name+beer_style+beer_abv, df.train),
               label = df.train$Recommended, max.depth = 20, eta = 1, nthread = 2, nrounds = 50, 
               objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Recommended~brewery_name+beer_style+beer_abv, df.train)), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Recommended~brewery_name+beer_style+beer_abv, df.train)) > 
                            sum(df.train$Recommended==1)/nrow(df.train))), as.factor(df.train$Recommended))
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Recommended~brewery_name+beer_style+beer_abv, df.test)) > 
                            sum(df.test$Recommended==1)/nrow(df.test))), as.factor(df.test$Recommended))
library(adabag)

## bagging
bag <- bagging(Recommended.factor~brewery_name+beer_style+beer_abv, df.train)
# variable importance
importanceplot(bag, cex.names=0.7)
# training
bag_pred_train <- predict(bag, newdata=df.train, type="class")
1-bag_pred_train$error

# validation
bag_pred_test <- predict(bag, newdata=df.test, type="class")
1-bag_pred_test$error

## random forest
rf <- randomForest(Recommended.factor~brewery_name+beer_style+beer_abv, df.train, method = "class")
# variable importance
varImp(rf, conditional=TRUE)

#TRAINING
rf_pred_train <- predict(rf, newdata=df.train, type="class")
confusionMatrix(df.train$Recommended.factor, rf_pred_train)
# validation
rf_pred_test <- predict(rf, newdata=df.test, type="class")
confusionMatrix(df.test$Recommended.factor, rf_pred_test)


# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# Below is Jon's code. 
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

##Here is the random forest piece of the code. I have ntree set pretty low
##right now for the initial runs. Need to bump back up to 89 or so.
rf.cv = train(review_overall ~ +brewery_name+beer_style+beer_abv,
              data = df,
              method="rf", nodesize=10, ntree=15,
              importance = TRUE,
              trControl=trainControl(method="cv", number=5),
              tuneGrid=data.frame(mtry=seq(20,30,1)))
rf.cv

#Plotting mtry for random forest
ggplot(rf.cv$results, aes(x=mtry, y=Rsquared)) +
  geom_point(size=3) +
  theme_bw() +
  ylab("Cross-Validation R^2") +
  scale_x_continuous(breaks=1:10, name="mtry") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))
ggsave("sample_graphs/R2_cp_RF.png")

