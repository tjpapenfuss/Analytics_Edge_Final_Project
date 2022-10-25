# ----------------------------------------------------------------------------------------
##### Problem 3: eBay.com
# ----------------------------------------------------------------------------------------
library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(ggplot2)

ebay.df <- read.xls("eBayAuctions.xls")
table(ebay.df$Competitive.)

ebay.df$category.factor <- as.factor(ebay.df$Category)
ebay.df$currency.factor <- as.factor(ebay.df$currency)
ebay.df$endDay.factor <- as.factor(ebay.df$endDay)
ebay.df$duration.factor <- as.factor(ebay.df$Duration)
ebay.df$competitive.factor <- as.factor(ebay.df$Competitive.)



set.seed(15071)
split = createDataPartition(ebay.df$category.factor, p = 0.6, list = FALSE) 
ebay.train = ebay.df[split,]
ebay.test = ebay.df[-split,]
head(ebay.train)
# str(ebay.train)
# Testing to make sure I have an appropriate distro. 
ggplot(aes(x=category.factor, y = ..count..), data = ebay.train) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))