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


getwd()
beer.df = read.csv("beer_reviews.csv")

cortest.df = beer.df[c("review_overall", "review_aroma", "review_appearance",
                     "review_palate", "review_taste", "beer_abv")]
str(cortest.df)

cor(cortest.df)                                                        
