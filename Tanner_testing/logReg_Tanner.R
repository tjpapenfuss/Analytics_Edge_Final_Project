library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(stringr)
library(ggplot2)
library(plotROC)

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
                                                   "Deschutes Brewery", 
                                                   "Three Floyds Brewing Co. & Brewpub",
                                                   "Avery Brewing Company",
                                                   "Great Divide Brewing Company",
                                                   "Victory Brewing Company",
                                                   "Goose Island Beer Co.",
                                                   "New Belgium Brewing",
                                                   "Anheuser-Busch",
                                                   "Tröegs Brewing Company",
                                                   "Port Brewing Company / Pizza Port",
                                                   "Bear Republic Brewing Co.","Mikkeller ApS",
                                                   "Smuttynose Brewing Company",
                                                   "Terrapin Beer Company", 
                                                   "AleSmith Brewing Company",
                                                   "Ballast Point Brewing Company",
                                                   "Cigar City Brewing",
                                                   "Weyerbacher Brewing Co.",
                                                   "Magic Hat Brewing Company",
                                                   "Shmaltz Brewing Company",
                                                   "Green Flash Brewing Co.",
                                                   "Great Lakes Brewing Company",
                                                   "Flying Dog Brewery", 
                                                   "Harpoon Brewery",
                                                   "Surly Brewing Company",
                                                   "Anchor Brewing Company",
                                                   "Full Sail Brewery & Tasting Room & Pub",
                                                   "Boulder Beer / Wilderness Pub",
                                                   "Pabst Brewing Company",
                                                   "Firestone Walker Brewing Co.",
                                                   "Oskar Blues Grill & Brew",
                                                   "Miller Brewing Co.",
                                                   "Boulevard Brewing Co.",
                                                   "The Lost Abbey",
                                                   "BrewDog",
                                                   "Matt Brewing Company",
                                                   "Ithaca Beer Company",
                                                   "Tyranena Brewing Company",
                                                   "New Holland Brewing Company",
                                                   "Breckenridge Brewery",
                                                   "Odell Brewing Company",
                                                   "Brasserie Dieu Du Ciel",
                                                   "Dark Horse Brewing Company",
                                                   "Short's Brewing Company",
                                                   "Alpine Beer Company",
                                                   "Otter Creek Brewing / Wolaver's",
                                                   "Redhook Ale Brewery",
                                                   "Widmer Brothers Brewing Company",
                                                   "Coors Brewing Company",
                                                   "Heavy Seas Beer",
                                                   "Brooklyn Brewery",
                                                   "New Glarus Brewing Company",
                                                   "Central Waters Brewing Company",
                                                   "Mendocino Brewing Company",
                                                   "Hair of the Dog Brewing Company / Brewery and Tasting Room",
                                                   "Anderson Valley Brewing Company",
                                                   "Genesee Brewing Co. / Dundee Brewing Co.",
                                                   "Stoudts Brewing Co.",
                                                   "Yuengling Brewery",
                                                   "North Coast Brewing Co.",
                                                   "Moylan's Brewery",
                                                   "Big Sky Brewing Company",
                                                   "Highland Brewing",
                                                   "SweetWater Brewing Company",
                                                   "Kona Brewing Co."
                                                   ))


#A question we need to ask. Do we want to split the data on the beer styles?
#Or do we split data based on the brewery? How do we want to present the data?
set.seed(123)
split = createDataPartition(df.filter2$review_overall, p = 0.70, list = FALSE) 
beer.train = df.filter2[split, ]
beer.test = df.filter2[-split, ]

#Subset the data frame to only use columns we are interested in using as predictors
df = subset(beer.train, select = -c(review_time,review_taste, review_palate,
                                   beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))

df.test = subset(beer.test, select = -c(review_time,review_taste, review_palate,
                                    beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))


#Set the categorical variables -->Check the resulting class of variables
df$beer_style = as.factor(df$beer_style)
df$brewery_name = as.factor(df$brewery_name)
str(df)

#Normalizing between 0 and 1 for Linear regression
df$review_overall<-ifelse(df$review_overall>= 4,1,0)
head(df)
df
table(df$review_overall)

#Logistic regression

logRegModel <- glm(review_overall ~ + brewery_name + beer_style + beer_abv, data = df, family="binomial")
logRegModel

#Prediction in the train set
pred = predict(logRegModel, newdata=df, type="response")
head(pred)
pred= ifelse(pred>0.5,1,0)
Accuracy = mean(pred!= df$review_overall)
print(1- Accuracy)
#[0.7320412]

#Normalizing in the test set
df.test$review_overall<-ifelse(df.test$review_overall>= 4,1,0)
head(df.test)
df.test
table(df.test$review_overall)

#Prediction in the Test dataset
pred.test = predict(logRegModel, newdata=df.test, type="response")
head(pred.test)
#pred.test= ifelse(pred.test>0.5,1,0)
Accuracy.test = mean(pred.test!= df.test$review_overall)
print(1- Accuracy.test)
#[0.7316206]


df.test$review_overall = as.factor(as.integer(df.test$review_overall))
library(ROCR)
# p <- predict(model, newdata=subset(df.test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(pred.test, df.test$review_overall)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

0.8647186 #(to be deleted or updated)

#plotting ROC and determining AUC using code straight from lecture
library(ROCR)
library(ggplot2)
pred.test = predict(logRegModel, newdata=df.test, type="response")
rocr.pred = prediction(pred.test, df.test$review_overall)
rocr.pred.df = data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")
                          [[1]],tpr=slot(performance(rocr.pred, "tpr", "fpr"),
                                         "y.values")[[1]])

print(ggplot(rocr.pred.df,aes(x=fpr)) +
        geom_line(aes(y=tpr),lwd=1) +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") +
        theme_bw() +
        xlim(0, 1) +
        ylim(0, 1) +
        theme(axis.title=element_text(size=30), axis.text=element_text(size=20)))

performance(rocr.pred, "auc")@y.values[[1]]


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# Below I am running a logistic regression model with top beers and reviewers. 
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

str(beer.df)

beer_top_profiles = beer.df %>% group_by(review_profilename) %>%
  filter(n() >= 50)
# Get the top beers by number of ratings. 
beer_top_beers = beer.df %>% group_by(beer_beerid) %>%
  filter(n() >= 50)
beer_top_styles = beer.df %>% group_by(beer_style) %>%
  filter(n() >= 1000)
beer_top_breweries = beer.df %>% group_by(brewery_name) %>%
  filter(n() >= 1000)


# Get the unique counts of the profiles and beers. 
uniqueID.profiles <- unique(beer_top_profiles$review_profilename)
uniqueID.beers <- unique(beer_top_beers$beer_beerid)
uniqueID.breweries <- unique(beer_top_breweries$brewery_name)
uniqueID.style <- unique(beer_top_styles$beer_style)
# Remove all the profiles and beers that are small in number. 
beer_top_profiles <- beer.df[beer.df$review_profilename %in% uniqueID.profiles[], , drop = FALSE]
beer_top_breweries <- beer_top_profiles[beer_top_profiles$brewery_name %in% uniqueID.breweries[], , drop = FALSE]
beer_top_styles <- beer_top_breweries[beer_top_breweries$beer_style %in% uniqueID.style[], , drop = FALSE]
beer_final <- beer_top_styles[beer_top_styles$beer_beerid %in% uniqueID.beers[], , drop = FALSE]
rm(beer_top_profiles)
rm(beer_top_breweries)
rm(beer_top_styles)
rm(beer_top_beers)

set.seed(123)
split = createDataPartition(beer_final$review_overall, p = 0.70, list = FALSE) 
beer.train = beer_final[split, ]
beer.test = beer_final[-split, ]
rm(beer_final)
#Subset the data frame to only use columns we are interested in using as predictors
beer.train = subset(beer.train, select = -c(review_time,review_taste, review_palate,
                                    beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))

beer.test = subset(beer.test, select = -c(review_time,review_taste, review_palate,
                                        beer_name,beer_beerid, review_profilename,
                                        review_aroma,review_appearance, brewery_id))


#Set the categorical variables -->Check the resulting class of variables
#df$beer_style = as.factor(df$beer_style)
#df$brewery_name = as.factor(df$brewery_name)
str(beer.train)

#Normalizing between 0 and 1 for Linear regression
beer.train$review_overall<-ifelse(beer.train$review_overall>= 4,1,0)
table(beer.train$review_overall)

#Logistic regression
library(ROCR)
logRegModel <- glm(review_overall ~ + brewery_name + beer_style + beer_abv, data = beer.train, family="binomial")
logRegModel

#Prediction in the train set
pred = predict(logRegModel, data=beer.train, type="response")
head(pred)
pred.0.5=ifelse(pred>0.5,1,0)
Accuracy = mean(pred.0.5!= beer.train$review_overall)
print(1- Accuracy)
#[0.7320412]

#Normalizing in the test set
beer.test$review_overall<-ifelse(beer.test$review_overall>= 4,1,0)
table(beer.test$review_overall)

#Prediction in the Test dataset
pred.test = predict(logRegModel, newdata=beer.test, type="response")
head(pred.test)
pred.test.0.5 = ifelse(pred.test>0.5,1,0)
pred.test.0.8 = ifelse(pred.test>0.8,1,0)
Accuracy.test = mean(pred.test.0.5!= beer.test$review_overall)
print(1- Accuracy.test)
#[0.7316206]
pr0.5 <- prediction(pred.test.0.5, beer.test$review_overall)
pr0.8 <- prediction(pred.test.0.8, beer.test$review_overall)
prf0.5 <- performance(pr0.5, measure = "tpr", x.measure = "fpr")
prf0.8 <- performance(pr0.8, measure = "tpr", x.measure = "fpr")

confusion.matrix = table(beer.test$review_overall, pred.test.0.8)
confusion.matrix
Accuracy.test = mean(pred.test.0.8!= beer.test$review_overall)

TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,])
TPR

FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,])
FPR

beer.test$review_overall = as.factor(as.integer(beer.test$review_overall))

# p <- predict(model, newdata=subset(beer.test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(pred.test, beer.test$review_overall)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
confusionMatrix(beer.test$review_overall, as.factor(pred.test.0.5))
confusionMatrix(beer.test$review_overall, as.factor(pred.test.0.8))
#plotting ROC and determining AUC using code straight from lecture
library(ROCR)
library(ggplot2)
pred.test = predict(logRegModel, newdata=beer.test, type="response")
rocr.pred = prediction(pred.test, beer.test$review_overall)
rocr.pred.df = data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")
                          [[1]],tpr=slot(performance(rocr.pred, "tpr", "fpr"),
                                         "y.values")[[1]])

print(ggplot(rocr.pred.df,aes(x=fpr)) +
        geom_line(aes(y=tpr),lwd=1) +
        geom_abline() +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") +
        ggtitle("ROC curve") +
        theme_bw() +
        xlim(0, 1) +
        ylim(0, 1) +
        theme(plot.title=element_text(size=25, hjust=0.4),axis.title=element_text(size=15), axis.text=element_text(size=15)))

performance(rocr.pred, "auc")@y.values[[1]]



