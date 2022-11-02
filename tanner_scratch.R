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
profiles_count #This gives the count of the beer styles

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

