#Importing all the packages that I need
library("dplyr")
library("tidyr")
library("ggplot2")
library("ROCR")
library("rpart")
library("rpart.plot")
library("randomForest")
library("tm")
library("SnowballC")
library("caret")
library("tidyverse")
library("gdata")

library("data.table")

#Setting Directory
getwd()
beer.df = read.csv("top20brewers.csv")
beer.df = na.omit(beer.df)
str(beer.df)

#beer.new = beer.df[order(beer.df$review_overall, decreasing = TRUE), ]
#beer.new = data.table(beer.new, key = "brewery_name")
#beer.new = beer.new[ , head(.SD,10), by = brewery_name ]
#beer.new

#Cleaning up data and checking for correlation. Believe it or not but there
#is not that much correlation between the 'opverall_' colums.
#cortest.df = beer.df[c("review_overall", "review_aroma", "review_appearance",
                     #"review_palate", "review_taste", "beer_abv")]
str(cortest.df)
#cor(cortest.df)                                                        



set.seed(15071)
split = createDataPartition(beer.df$review_overall, p = 0.75, list = FALSE) 
beer.train = beer.df[split,]
beer.test = beer.df[-split,]

df = subset(beer.test, select = -c(beer_name, brewery_name))

beer.mod = rpart(review_overall~.,  data = df, 
                 method="class", cp=0.2)
