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
beer.df = read.csv("~/AE-FinalProj-data/beer_reviews.csv")
count(beer.df)
# The below code will remove the NaNs. 
beer.df = na.omit(beer.df)
beer.df$beer_style.factor = as.factor(beer.df$beer_style)

summary(beer.df)
str(beer.df)



#A question we need to ask. Do we want to split the data on the beer styles?
#Or do we split data based on the brewery? How do we want to present the data?
set.seed(123)
split = createDataPartition(beer.df$beer_style.factor, p = 0.7, list = FALSE) 
beer.train = beer.df[split,]
beer.test = beer.df[-split,]
head(beer.train)

# str(ebay.train)
# Plotting the beer style counts. This is all beer styles
ggplot(aes(x=beer_style.factor, y = ..count..), data = beer.df) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#This gets the count of each unique beer style
style_count = beer.df %>% count(beer_style, sort = TRUE)
style_count #This gives the count of the beer styles

#This gets the nubmer of reviews for each unique profile name
profiles_count = beer.df %>% count(review_profilename, sort = TRUE)
profiles_count #This gives the count of the beer styles

#Filter beer by American IPA. This gets rid of all beers except American IPA.
beer.ipa = filter(beer.train, beer_style == "American IPA")
head(beer.apa)

# Warning this pot is very convoluted. 
ggplot(aes(x=brewery_name, y = ..count..), data = beer.ipa) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Get the unique breweries that create IPAs. 
brewery_count = beer.ipa %>% count(brewery_name, sort = TRUE)
brewery_count #This gives the count of the beer styles

ggplot(beer.ipa, aes(x = brewery_name, y = review_overall)) + geom_point() +
  ggtitle("Age vs. Readmission") + xlab("Age") + ylab("Readmission")

regressor = lm(review_overall ~ brewery_name, data = beer.ipa)
summary(regressor)





