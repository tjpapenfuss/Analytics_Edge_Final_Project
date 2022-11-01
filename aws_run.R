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
beer.df = read.csv("AE-FinalProj-data/beer_reviews.csv")
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
beer.train = beer.df[split, ]
beer.test = beer.df[-split, ]
head(beer.train)

start_time <- Sys.time()
print("CART Starting.")

tree <- rpart(review_overall ~ ., data = beer.train, method="class", cp=0.001)
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
