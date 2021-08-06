#STAT 6021 
#Project 2

library(tidyverse)
library(corrr)

#set working directory
#read in whites data and add column with color of wine
whites<- read.csv('wineQualityWhites.csv', header=TRUE, sep=",")
whites$color_of_wine<- 1
#1 for whites
#read in reads data and add column with color of wine
reds<-read.csv('wineQualityReds.csv', header= TRUE, sep=',')
reds$color_of_wine<- 0
#0 for reds (reference class)

#merge them since have same variables
wines <- rbind(whites, reds)

#take a look at the data
head(wines)
tail(wines)

# splits all wine data into training and testing, 50% train, 50% test
# good idea to set.seed for now so we are all on same page with data
set.seed(69) #for if we want the split to be the same each time
train_test_split<-sample.int(nrow(wines), floor(.5*nrow(wines)), replace = F)
train<-wines[train_test_split, ]
test<-wines[-train_test_split, ]



#Print scatterplot matrix - too small? 
pairs(train, lower.panel=NULL)

cor(train)

#correlations matrix- only for MLR to see if predictors are related
num.wines <- train %>% 
  select(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide,total.sulfur.dioxide, density, pH, sulphates, alcohol, quality)
res.cor <- correlate(num.wines)
res.cor
#fixed acidity & density (.46)
#density & residual sugar (.54)
#total sulfur & residual sugar (.5)
#free & total sulfur (.72)
#density & alcohol (-.7)
#quality & alcohol (.45)

##have R treat color of wine as categorical
##check coding scheme
#not sure if this needs to be done for each data set???
train$color_of_wine<-factor(train$color_of_wine) 
is.factor(train$color_of_wine) 
contrasts(train$color_of_wine)
levels(train$color_of_wine)
#red = 0 white=1

test$color_of_wine<-factor(test$color_of_wine) 
is.factor(test$color_of_wine) 
contrasts(test$color_of_wine)
levels(test$color_of_wine)



#get an idea of summaries
summary(train)
str(train)

#how many in each class
nrow(train[train$color_of_wine == 0,]) #1459 reds
nrow(train[train$color_of_wine ==1,])  #4388 whites

#=============== EDA ===========================

#Quality against color -> look them same- not a good predictor
boxplot(train$quality~train$color_of_wine, xlab='Color of Wine', ylab='Quality Rating', main='Quality Rating by Wine Color')

#further inspection of distribution of quality overall and by wine color
hist(train$quality, xlab='Quality', ylab='Count', main='Quality')
#roughly normal
print('testing git')
##test
print('test')


#fixed acidity against color -> red wines general higher fixed acidity
boxplot(train$fixed.acidity~train$color_of_wine, xlab='Color of Wine', ylab='Fixed Acidity', main='Fixed Rating by Wine Color')

#Volatile Acidity against color -> reds wines generally have higher volatile acidity
boxplot(train$volatile.acidity~train$color_of_wine, xlab='Color of Wine', ylab='Volatile Acidity', main='Volatile Acidity by Wine Color')

#Citric Acid against color -> reds wines have more variance in citric acid
#white wines have smaller range although many outliers???
boxplot(train$citric.acid~train$color_of_wine, xlab='Color of Wine', ylab='Citric Acid', main='Citric by Wine Color')

#Residual Sugar against color -> white wines generally have more sugar
boxplot(train$residual.sugar~train$color_of_wine, xlab='Color of Wine', ylab='Redsidual Sugar', main='Residual Sugar by Wine Color')

#Chlorides against color -> reds wines generally have more chlorides
boxplot(train$chlorides~train$color_of_wine, xlab='Color of Wine', ylab='Chlorides', main='Chlorides by Wine Color')

#Free Sulfur dioxides against color -> white wines generally have more free sulfur dioxide
boxplot(train$free.sulfur.dioxide~train$color_of_wine, xlab='Color of Wine', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Wine Color')

#Total sulfur dioxide against color -> white wines generally have more total sulfur dioxide
boxplot(train$total.sulfur.dioxide~train$color_of_wine, xlab='Color of Wine', ylab='Total Sulfur Dioxide', main='Total Sulfur Dioxide by Wine Color')

#Density against color -> reds wines generally have higher density
boxplot(train$density~train$color_of_wine, xlab='Color of Wine', ylab='Density', main='Density by Wine Color')

#pH against color -> reds wines generally have higher pH -> whites are more acidic
#all wines in dataset are acidic (pH < 7)
boxplot(train$pH~train$color_of_wine, xlab='Color of Wine', ylab='pH', main='pH by Wine Color')

#Sulphates against color -> reds wines generally have more sulphates 
boxplot(train$sulphates~train$color_of_wine, xlab='Color of Wine', ylab='Sulphates', main='Sulphates by Wine Color')

#Alcohol against color -> roughly the same (little distinction between colors)
boxplot(train$alcohol~train$color_of_wine, xlab='Color of Wine', ylab='Alchol', main='Alcohol by Wine Color')


#I got crazy with box plots. Quality is the response variable but trying to 
#determine how best to handle it.


#Fixed Acidity against quality -> not much of a pattern? 
boxplot(train$fixed.acidity~train$quality, xlab='Quality', ylab='Fixed Acidity', main='Fixed Acidity by Quality')
hist(train$fixed.acidity) #skewed right

#Volatile Acidity against quality -> as quality increases, volatile acidity decreases
boxplot(train$volatile.acidity~train$quality, xlab='Quality', ylab='Volatile Acidity', main='Volatile Acidity by Quality')
hist(train$volatile.acidity) #skewed right

#Citric Acid against quality -> not much of a pattern
boxplot(train$citric.acid~train$quality, xlab='Quality', ylab='Citric Acid', main='Citrtic Acid by Quality')
hist(train$citric.acid) #roughly normal? 


#Residual Sugar against quality -> no pattern? hard to tell
boxplot(train$residual.sugar~train$quality, xlab='Quality', ylab='Residual Sugar', main='Residual Sugar by Quality')
hist(train$residual.sugar) #skewed right

#Chlorides against quality -> as quality increases, volatile acidity decreases
boxplot(train$chlorides~train$quality, xlab='Quality', ylab='Chlorides', main='Chlorides by Quality')
hist(train$chlorides) #skewed right


#Free Sulfur Dioxide against quality -> as quality increases, free sulphur dioxide increases?
boxplot(train$free.sulfur.dioxide~train$quality, xlab='Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Quality')
hist(train$free.sulfur.dioxide) #skewed right

#Total Sulfur Dioxide against quality -> as quality increases, variance of total sulfur decreases 
boxplot(train$total.sulfur.dioxide~train$quality, xlab='Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Quality')
hist(train$total.sulfur.dioxide) #skewed left?

#Volatile Acidity against quality -> as quality increases, volatile acidity decreases
boxplot(train$volatile.acidity~train$quality, xlab='Quality', ylab='Volatile Acidity', main='Volatile Acidity by Quality')
hist(train$volatile.acidity) #skewed right


#Density against quality -> as quality increases, density decreases
boxplot(train$density~train$quality, xlab='Quality', ylab='Density', main='Density by Quality')
hist(train$density) #normal?


#pH against quality -> no pattern? 
boxplot(train$pH~train$quality, xlab='Quality', ylab='pH', main='pH by Quality')
hist(train$pH) #normal


#sulphates against quality -> no pattern? normal?
boxplot(train$sulphates~train$quality, xlab='Quality', ylab='Sulphates', main='Sulphates by Quality')
hist(train$sulphates) # approximately normal skewed right

#alcohol against quality -> as quality increase, alcohol increases
boxplot(train$alcohol~train$quality, xlab='Quality', ylab='Alcohol', main='Alcohol by Quality')
hist(train$alcohol) #skewed right


##have R treat quality as categorical
#not sure if this needs to be donw for each data set???
train$quality<-factor(train$quality) 
is.factor(train$quality) 
test$quality<-factor(test$quality) 
is.factor(test$quality) 

##check coding scheme
contrasts(train$quality)
contrasts(test$quality)
levels(train$quality)
levels(test$quality)

##collapse 3-6 -> bad, 7-9 -> good
new.levels<-c("Bad", "Bad", "Bad", "Bad", "Good", "Good", "Good") ##need to match up with the order
train$quality.binary<-factor(new.levels[train$quality]) ##add this new binary variable to data frame
test$quality.binary<-factor(new.levels[test$quality]) ##add this new binary variable to data frame

#be sure there are 
nrow(train[train$quality.binary=="Good",]) #1140 in the training
nrow(test[test$quality.binary=="Good",]) #137 in the test

#recheck boxplots wioth binary qulaity
#Fixed Acidity against quality -> good wines generally lower fixed acidity
boxplot(train$fixed.acidity~train$quality.binary, xlab='Binary Quality', ylab='Fixed Acidity', main='Fixed Acidity by Binary Quality')


#Volatile Acidity against quality -> good wines lower volatile acidity
boxplot(train$volatile.acidity~train$quality.binary, xlab='Binary Quality', ylab='Volatile Acidity', main='Volatile Acidity by Binary Quality')


#Citric Acid against quality -> not much of a pattern?
boxplot(train$citric.acid~train$quality.binary, xlab='Binary uality', ylab='Citric Acid', main='Citric Acid by Binary Quality')



#Residual Sugar against quality -> no pattern? hard to tell
boxplot(train$residual.sugar~train$quality.binary, xlab='Binary Quality', ylab='Residual Sugar', main='Residual Sugar by Binary Quality')


#Chlorides against quality -> good wines less chlorides
boxplot(train$chlorides~train$quality.binary, xlab='Binary Quality', ylab='Chlorides', main='Chlorides by Binary Quality')



#Free Sulfur Dioxide against quality -> about the same
boxplot(train$free.sulfur.dioxide~train$quality.binary, xlab='Binary Quality', ylab='Free Sulfur Dioxide', main='Free Sulfur Dioxide by Binary Quality')


#Total Sulfur Dioxide against quality -> good wines have  less variance of total sulfur decreases 
boxplot(train$total.sulfur.dioxide~train$quality.binary, xlab='Binary Quality', ylab='Total Sulfur Dioxide', main='Total Sulfur Dioxide by Binary Quality')


#Density against quality -> good wines has lower density
boxplot(train$density~train$quality.binary, xlab='Binary Quality', ylab='Density', main='Density by Binary Quality')



#pH against quality -> same
boxplot(train$pH~train$quality.binary, xlab='Binary Quality', ylab='pH', main='pH by Binary Quality')



#sulphates against quality -> no pattern? 
boxplot(train$sulphates~train$quality.binary, xlab='Binary Quality', ylab='Sulphates', main='Sulphates by Binary Quality')

#alcohol against quality -> good wines have higher alcohol content
boxplot(train$alcohol~train$quality.binary, xlab='Binary Quality', ylab='Alcohol', main='Alcohol by Binary Quality')

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

