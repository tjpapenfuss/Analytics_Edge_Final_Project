library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(stringr)
library(ROCR)

# The below code reads in all beers.
beer.df = read.csv("beer_reviews.csv")
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
df = subset(beer.train, select = -c(review_time,review_taste, review_palate,
                                   beer_name,beer_beerid, review_profilename,
                                    review_aroma,review_appearance, brewery_id))

#binary.df = df
#binary.df$review_overall = if_else(binary.df$review_overall>4, 1, 0)
#str(binary.df)

#Set the categorical variables -->Check the resulting class of variables
#df$beer_style = as.factor(df$beer_style)
#df$brewery_name = as.factor(df$brewery_name)
str(df)



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

