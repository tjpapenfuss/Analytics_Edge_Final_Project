library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(stringr)

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
                                                   "Surly Brewing Company"
                                                   ))


#A question we need to ask. Do we want to split the data on the beer styles?
#Or do we split data based on the brewery? How do we want to present the data?
set.seed(123)
split = createDataPartition(df.filter2$review_overall, p = 0.70, list = FALSE) 
beer.train = df.filter2[split, ]
beer.test = df.filter2[-split, ]

#Subset the data frame to only use columns we are interested in using as predictors
df = subset(df.filter2, select = -c(review_time,review_taste, review_palate,
                                   beer_name,beer_beerid, review_profilename,
                                   review_aroma, review_appearance, brewery_id))

#Set the categorical variables -->Check the resulting class of variables
#df$beer_style = as.factor(df$beer_style)
#df$brewery_name = as.factor(df$brewery_name)
str(df)

# Data frame @ 283k samples
# Standard linear regression [R2=0.13]
regressor = lm(review_overall ~ ., data = df)
summary(regressor)

#CART Model --> HOLY SHIT!!! We have a tree!
#Minbucket not really helping
tree = rpart(review_overall ~ ., data = df, method="class", 
             cp=0.01)
prp(tree, varlen=0,faclen=0,digits=3)

#Now on to assess the performance on the test set.
#Bringing in some functions for this.
calc.OSR2 = function(m.preds, tst, trn)
  return( 1 - sum((m.preds - tst)^2) / sum((mean(trn) - tst)^2))
calc.MAE = function(m.preds, tst)
  return(mean(abs(m.preds-tst)))
calc.RMSE = function(m.preds, tst)
  return(sqrt(mean((m.preds-tst)^2)))

#Doing a prediction on the test set. There may be a better way to do this... [R2 = -332 :( ]
t.pred = predict(tree, newdata = df)
calc.OSR2(t.pred, df$review_overall, df$review_overall)


#Lets try cross validation now to see if we can salvage this sinking ship
#Couldn't get it to work initiall. I am going to re-name the dataframe.
cv.tree = train(review_overall ~ +brewery_name+beer_style+beer_abv,
                 data = df,
                 method="rpart",
                 tuneGrid=data.frame(cp=seq(0, 0.0005, 0.0001)),
                 trControl=trainControl(method="cv", number=10))
cv.tree #The results recommend cp = 0? Is this right?

#Plotting the results of cp
ggplot(cv.tree$results, aes(x=cp, y=Rsquared)) +
        geom_point(size=3) +
        theme_bw() +
        ylab("Cross-Validation R^2") +
        scale_x_continuous(breaks=0:1, name="cp") +
        theme(axis.title=element_text(size=18), axis.text=element_text(size=18))
ggsave("sample_graphs/R2_cp_CART.png")


##Here is the random forest piece of the code. I have ntree set pretty low
##right now for the initial runs. Need to bump back up to 89 or so.
rf.cv = train(review_overall ~ +brewery_name+beer_style+beer_abv,
              data = df,
              method="rf", nodesize=25, ntree=80,
              importance = TRUE,
              trControl=trainControl(method="cv", number=5),
              tuneGrid=data.frame(mtry=seq(10,20,1)))
rf.cv

#Plotting mtry for random forest
#print(ggplot(rf.cv$results, aes(x=mtry, y=Rsquared)) +
        #geom_point(size=3) +
        #theme_bw() +
        #ylab("Cross-Validation R^2") +
        #scale_x_continuous(breaks=1:10, name="mtry") +
        #theme(axis.title=element_text(size=18), axis.text=element_text(size=18)))

