#STAT 6021 
#Project 2

library(tidyverse)
library(corrr)

#set working directory
#read in whites data and add column with color of wine
whites<- read.csv('wineQualityWhites.csv', header=TRUE, sep=",")
whites$color_of_wine<- 'white'

#look at summary of whites and plot a boxplot
summary(whites)
boxplot(whites$quality, main='White Wine Quality Ratings')

#read in reads data and add column with color of wine
reds<-read.csv('wineQualityReds.csv', header= TRUE, sep=',')
reds$color_of_wine<- 'red'

##look at summary of reds and plot a boxplot
summary(reds)
boxplot(reds$quality, main='Red Wine Quality Ratings')


#merge them since have same variables
wines <- rbind(whites, reds)

#take a look at the data
head(wines)
tail(wines)

#get an idea of summaries
summary(wines)
str(wines)
#be sure whine color is a factor
wines$color_of_wine<-factor(wines$color_of_wine)
levels(wines$color_of_wine)
contrasts(wines$color_of_wine)
#red is the reference

#=============== EDA ===========================

#Quality against color -> look them same- not a good predictor
boxplot(wines$quality~wines$color_of_wine, xlab='Color of Wine', ylab='Quality Rating', main='Quality Rating by Wine Color')

#further inspection of distribution of quality overall and by wine color
hist(wines$quality, xlab='Quality', ylab='Count', main='Quality')
hist(whites$quality, xlab='Quality', ylab='Count', main='Quality of White Wines')
hist(reds$quality, xlab='Quality', ylab='Count', main='Quality of Red Wines')

#fixed acidity against color -> red wines general higher fixed acidity
boxplot(wines$fixed.acidity~wines$color_of_wine, xlab='Color of Wine', ylab='Fixed Acidity', main='Fixed Rating by Wine Color')

#Volatile Acidity against color -> reds wines generally have higher volatile acidity
boxplot(wines$volatile.acidity~wines$color_of_wine, xlab='Color of Wine', ylab='Volatile Acidity', main='Volatile Acidity by Wine Color')

#Citric Acid against color -> reds wines have more variance in citric acid
#white wines have smaller range 
boxplot(wines$citric.acid~wines$color_of_wine, xlab='Color of Wine', ylab='Citric Acid', main='Citric by Wine Color')

#Residual Sugar against color -> white wines generally have more sugar
boxplot(wines$residual.sugar~wines$color_of_wine, xlab='Color of Wine', ylab='Redsidual Sugar', main='Residual Sugar by Wine Color')

#Chlorides against color -> reds wines generally have more chlorides
boxplot(wines$chlorides~wines$color_of_wine, xlab='Color of Wine', ylab='Chlorides', main='Chlorides by Wine Color')

#Free Sulfur dioxides against color -> white wines generally have more free sulfur dioxide
boxplot(wines$free.sulfur.dioxide~wines$color_of_wine, xlab='Color of Wine', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Wine Color')

#Total sulfur dioxide against color -> white wines generally have more total sulfur dioxide
boxplot(wines$total.sulfur.dioxide~wines$color_of_wine, xlab='Color of Wine', ylab='Total Sulfur Dioxide', main='Total Sulfur Dioxide by Wine Color')

#Density against color -> reds wines generally have higher density
boxplot(wines$density~wines$color_of_wine, xlab='Color of Wine', ylab='Density', main='Density by Wine Color')

#pH against color -> reds wines generally have higher pH -> whites are more acidic
#all wines in dataset are acidic (pH < 7)
boxplot(wines$pH~wines$color_of_wine, xlab='Color of Wine', ylab='pH', main='pH by Wine Color')

#Sulphates against color -> reds wines generally have more sulphates 
boxplot(wines$sulphates~wines$color_of_wine, xlab='Color of Wine', ylab='Sulphates', main='Sulphates by Wine Color')

#Alcohol against color -> roughly the same (little distinction between colors)
boxplot(wines$alcohol~wines$color_of_wine, xlab='Color of Wine', ylab='Alchol', main='Alcohol by Wine Color')


#I got crazy with box plots. Quality is the response variable but trying to 
#determine how best to handle it.


#Fixed Acidity against quality -> not much of a pattern? 
boxplot(wines$fixed.acidity~wines$quality, xlab='Quality', ylab='Fixed Acidity', main='Fixed Acidity by Quality')
hist(wines$fixed.acidity) #skewed right

#Volatile Acidity against quality -> as quality increases, volatile acidity decreases
boxplot(wines$volatile.acidity~wines$quality, xlab='Quality', ylab='Volatile Acidity', main='Volatile Acidity by Quality')
hist(wines$volatile.acidity) #skewed right

#Citric Acid against quality -> not much of a pattern
boxplot(wines$citric.acid~wines$quality, xlab='Quality', ylab='Citric Acid', main='Citrtic Acid by Quality')
hist(wines$citric.acid) #roughly normal? 


#Residual Sugar against quality -> no pattern? hard to tell
boxplot(wines$residual.sugar~wines$quality, xlab='Quality', ylab='Residual Sugar', main='Residual Sugar by Quality')
hist(wines$residual.sugar) #skewed right

#Chlorides against quality -> as quality increases, volatile acidity decreases
boxplot(wines$chlorides~wines$quality, xlab='Quality', ylab='Chlorides', main='Chlorides by Quality')
hist(wines$chlorides) #skewed right


#Free Sulfur Dioxide against quality -> as quality increases, free sulphur dioxide increases?
boxplot(wines$free.sulfur.dioxide~wines$quality, xlab='Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Quality')
hist(wines$free.sulfur.dioxide) #skewed right

#Total Sulfur Dioxide against quality -> as quality increases, variance of total sulfur decreases 
boxplot(wines$total.sulfur.dioxide~wines$quality, xlab='Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Quality')
hist(wines$total.sulfur.dioxide) #skewed left?

#Volatile Acidity against quality -> as quality increases, volatile acidity decreases
boxplot(wines$volatile.acidity~wines$quality, xlab='Quality', ylab='Volatile Acidity', main='Volatile Acidity by Quality')
hist(wines$volatile.acidity) #skewed right


#Density against quality -> as quality increases, density decreases
boxplot(wines$density~wines$quality, xlab='Quality', ylab='Density', main='Density by Quality')
hist(wines$density) #normal?


#pH against quality -> no pattern? 
boxplot(wines$pH~wines$quality, xlab='Quality', ylab='pH', main='pH by Quality')
hist(wines$pH) #normal


#sulphates against quality -> no pattern? normal?
boxplot(wines$sulphates~wines$quality, xlab='Quality', ylab='Sulphates', main='Sulphates by Quality')
hist(wines$sulphates) # approxiamtely normal skewed right

#alcohol against quality -> as quality increase, alcohol increases
boxplot(wines$alcohol~wines$quality, xlab='Quality', ylab='Alcohol', main='Alcohol by Quality')
hist(wines$alcohol) #skewed right



# splits all wine data into training and testing, 90% train, 10% test
# good idea to set.seed for now so we are all on same page with data
set.seed(69) #for if we want the split to be the same each time
train_test_split<-sample.int(nrow(wines), floor(.9*nrow(wines)), replace = F)
train<-wines[train_test_split, ]
test<-wines[-train_test_split, ]



#Print scatterplot matrix - too small? 
pairs(wines, lower.panel=NULL)

#correlations matrix- only for MLR to see if predictors are related
num.wines <- wines %>% 
  select(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide,total.sulfur.dioxide, density, pH, sulphates, alcohol, quality)
res.cor <- correlate(num.wines)
res.cor
#fixed acidity & density (.46)
#density & residual sugar (.55)
#total sulfur & residual sugar (.5)
#free & total sulfur (.72)
#density & alcohol (-.69)
#quality & alcohol (.44)

##have R treat quality as categorical
#not sure if this needs to be donw for each data set???
train$quality<-factor(train$quality) 
is.factor(train$quality) 
test$quality<-factor(test$quality) 
is.factor(test$quality) 
wines$quality<-factor(wines$quality) 
is.factor(wines$quality)
##check coding scheme
contrasts(train$quality)
contrasts(test$quality)
levels(train$quality)
levels(test$quality)
contrasts(wines$quality)
levels(wines$quality)
##collapse 3-6 -> bad, 7-9 -> good
new.levels<-c("Bad", "Bad", "Bad", "Bad", "Good", "Good", "Good") ##need to match up with the order
train$quality.binary<-factor(new.levels[train$quality]) ##add this new binary variable to data frame
test$quality.binary<-factor(new.levels[test$quality]) ##add this new binary variable to data frame
wines$quality.binary<-factor(new.levels[wines$quality]) ##add this new binary variable to data frame

#be sure there are 
nrow(wines[wines$quality.binary=="Good",]) #1277 from all data
nrow(train[train$quality.binary=="Good",]) #1140 in the training
nrow(test[test$quality.binary=="Good",]) #137 in the test

#recheck boxplots wioth binary qulaity
#Fixed Acidity against quality -> good wines generally lower fixed acidity
boxplot(wines$fixed.acidity~wines$quality.binary, xlab='Binary Quality', ylab='Fixed Acidity', main='Fixed Acidity by Binary Quality')


#Volatile Acidity against quality -> good wines lower volatile acidity
boxplot(wines$volatile.acidity~wines$quality.binary, xlab='Binary Quality', ylab='Volatile Acidity', main='Volatile Acidity by Binary Quality')


#Citric Acid against quality -> not much of a pattern?
boxplot(wines$citric.acid~wines$quality.binary, xlab='Binary uality', ylab='Citric Acid', main='Citric Acid by Binary Quality')



#Residual Sugar against quality -> no pattern? hard to tell
boxplot(wines$residual.sugar~wines$quality.binary, xlab='Binary Quality', ylab='Residual Sugar', main='Residual Sugar by Binary Quality')


#Chlorides against quality -> good wines less chlorides
boxplot(wines$chlorides~wines$quality.binary, xlab='Binary Quality', ylab='Chlorides', main='Chlorides by Binary Quality')



#Free Sulfur Dioxide against quality -> about the same
boxplot(wines$free.sulfur.dioxide~wines$quality.binary, xlab='Binary Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Binary Quality')


#Total Sulfur Dioxide against quality -> good wines have  less variance of total sulfur decreases 
boxplot(wines$total.sulfur.dioxide~wines$quality.binary, xlab='Binary Quality', ylab='Total Sulfur Dioxide', main='Total Sulfur Dioxide by Binary Quality')


#Density against quality -> good wines has lower density
boxplot(wines$density~wines$quality.binary, xlab='Binary Quality', ylab='Density', main='Density by Binary Quality')



#pH against quality -> same
boxplot(wines$pH~wines$quality.binary, xlab='Binary Quality', ylab='pH', main='pH by Binary Quality')



#sulphates against quality -> no pattern? 
boxplot(wines$sulphates~wines$quality.binary, xlab='Binary Quality', ylab='Sulphates', main='Sulphates by Binary Quality')

#alcohol against quality -> good wines have higher alcohol content
boxplot(wines$alcohol~wines$quality.binary, xlab='Binary Quality', ylab='Alcohol', main='Alcohol by Binary Quality')

# looks like alcohol, density, volatile acidity, chlorides? and citric acid may
# differentiate most between good and bad wines. 

#fit logistic model with proposed variables
results<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides+citric.acid, family= 'binomial', data= train)
summary(results) 

#is the model useful?
#check to see if B's = 0 
#testing all coefficients, use Delta G^2 test statistic
#df = 5846(df of null deviance) - 5841(res deviance for full) = 5
#H0: all Bs  = 0 (not useful)
#HA: at least one B non zero (useful)
#test stat delta G^2 = null deviance - residual devaince
5769.3-4680.6
#1088.7
#qchisq(1-alpha, df)
qchisq(.95, 5)
#critical value = 11.0705
#Delta G^2 > critical value -> reject null
#want areas to the right so subtract from 1
1-pchisq(results$null.deviance-results$deviance,5)
#p = 0 , p < 0.05, reject the null- data supports that at least one 
#of the coefficients is nonzero
#model is useful


#Wald Test- 
#H0: B citric acid = 0 (not useful)
#HA: B citric acid != 0 (useful)
#z-stat: -0.865
#p-value: 0.38710
#p>0.05, fail to reject the null
#citric acid not useful with other predictors in the model

#call library to use ROCR
library(ROCR)

#set up for roc (false postive rate on x axis, true postive rate on y axis)
preds<- predict(results, newdata=test, type='response')
rates<-prediction(preds, test$quality.binary)
roc_results<- performance(rates, measure = 'tpr', x.measure = 'fpr')
#plot
plot(roc_results)
lines(x=c(0,1), y = c(0,1), col= 'red')

#Because the ROC is above the diagonal (top left), this indicates that the model
#performs better than randomly guessing. 



#AUC (Area under the Curve)
auc <- performance(rates, measure = 'auc')
auc@y.values
#AUC = 0.7843229 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 



#confusion matrix
table(test$quality.binary, preds>0.5)
#overall error rate: 105 + 25 / 488 + 25 + 105 + 32 = 130/650 = .2 
#False Positive Rate: 25 / 488 + 25 = 25/513 = 0.04873
#False Negative Rate: 105 /105 + 32 = 105/137 = 0.766
#Sensitivity: 32 / 105 + 32 = 32/137 = 0.233
#Specificity:   488/488 + 25 = 0.95127

