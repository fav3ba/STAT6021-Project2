#STAT 6021 
#Project 2

library(tidyverse)
library(faraway)
library(ROCR)

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
#removed index column
wines <- subset(wines, select = -X )

#take a look at the data
head(wines)
tail(wines)

##have R treat color of wine as categorical
##check coding scheme
# moved factoring earlier in code to do all at once
wines$color_of_wine<-factor(wines$color_of_wine) 
is.factor(wines$color_of_wine) 
contrasts(wines$color_of_wine)
levels(wines$color_of_wine)
#red = 0 white=1


# splits all wine data into training and testing, 50% train, 50% test
# good idea to set.seed for now so we are all on same page with data
set.seed(69) #for if we want the split to be the same each time
train_test_split<-sample.int(nrow(wines), floor(.5*nrow(wines)), replace = F)
train<-wines[train_test_split, ]
test<-wines[-train_test_split, ]


#Print scatterplot matrix - too small? 
#can see a bit better now-> what do you think look the most correlated?
pairs(train, lower.panel=NULL)

cor(train)

#fixed acidity & density (.48)
#density & residual sugar (.53)
#total sulfur & residual sugar (.5)
#free & total sulfur (.72)
#density & alcohol (-.70)
#quality & alcohol (.44)

#get an idea of summaries
summary(train)
str(train)

#how many in each class
nrow(train[train$color_of_wine == 0,]) #819 reds
nrow(train[train$color_of_wine == 1,])  #2429 whites

#=============== EDA ===========================

#Quality against color -> look them same- not a good predictor
boxplot(train$quality~train$color_of_wine, xlab='Color of Wine', ylab='Quality Rating', main='Quality Rating by Wine Color')

#further inspection of distribution of quality overall and by wine color
hist(train$quality, xlab='Quality', ylab='Count', main='Wine Quality')
#roughly normal

#these are for comparison between the colors of wine 
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

#These are ones that matter more as our response is quality
#Fixed Acidity against quality -> not much of a pattern? 
boxplot(train$fixed.acidity~train$quality, xlab='Quality', ylab='Fixed Acidity', main='Fixed Acidity by Quality')
hist(train$fixed.acidity) #skewed right

#Volatile Acidity against quality -> as quality increases, volatile acidity decreases
boxplot(train$volatile.acidity~train$quality, xlab='Quality', ylab='Volatile Acidity', main='Volatile Acidity by Quality')
hist(train$volatile.acidity) #skewed right

#Citric Acid against quality -> not much of a pattern
boxplot(train$citric.acid~train$quality, xlab='Quality', ylab='Citric Acid', main='Citric Acid by Quality')
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
boxplot(train$total.sulfur.dioxide~train$quality, xlab='Quality', ylab='Total Sulfur Dioxide', main='Total Sulfur Dioxide by Quality')
hist(train$total.sulfur.dioxide) #skewed left?



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
new.levels<-c("Low", "Low", "Low", "Low", "QHigh", "QHigh", "QHigh") ##need to match up with the order
train$quality.binary<-factor(new.levels[train$quality]) ##add this new binary variable to data frame
test$quality.binary<-factor(new.levels[test$quality]) ##add this new binary variable to data frame
#low is the reference class - I used Qhigh so that Low would be reference- since
#high quality should be seen as success- easier to interpret later
summary(train)

#be sure there are similar amounts
nrow(train[train$quality.binary=="QHigh",]) #657 in the training
nrow(test[test$quality.binary=="QHigh",]) #620 in the test



#recheck boxplots with binary qulaity
#Fixed Acidity against quality -> good wines generally lower fixed acidity
boxplot(train$fixed.acidity~train$quality.binary, xlab='Binary Quality', ylab='Fixed Acidity', main='Fixed Acidity by Binary Quality')


#Volatile Acidity against quality -> good wines lower volatile acidity
boxplot(train$volatile.acidity~train$quality.binary, xlab='Binary Quality', ylab='Volatile Acidity', main='Volatile Acidity by Binary Quality')


#Citric Acid against quality -> not much of a pattern?
boxplot(train$citric.acid~train$quality.binary, xlab='Binary Quality', ylab='Citric Acid', main='Citric Acid by Binary Quality')



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



#================== start making the model ================================

#fit logistic model with proposed variables -> those most different by box plot
resultsavd<-glm(quality.binary~alcohol+volatile.acidity+density, family= 'binomial', data= train)
summary(resultsavd)

#is the model useful?
#check to see if B's = 0 
#testing all coefficients, use Delta G^2 test statistic
#df = 3247(df of null deviance) - 3244(res deviance for full) = 3
#H0: all Bs  = 0 (not useful)
#HA: at least one B non zero (useful)
#test stat delta G^2 = null deviance - residual deviance
resultsavd$null.deviance-resultsavd$deviance 
#595.99
#qchisq(1-alpha, df)
qchisq(.95, 3)
#critical value = 7.8147
#Delta G^2 > critical value -> reject null
#want areas to the right so subtract from 1
#testing all coefficients, use Delta G^2 test statistic
#df = 3247(df of null deviance) - 3244(res deviance for full) = 3
#H0: all Bs  = 0 (not useful)
#HA: at least one B non zero (useful)
1-pchisq(resultsavd$null.deviance-resultsavd$deviance,3)
#p = 0 , p < 0.05, reject the null- data supports that at least one 
#of the coefficients is nonzero
#model is useful

#wald test-> all significant -> dont drop any

#fit logistic model with proposed variables
results<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides+fixed.acidity+residual.sugar, family= 'binomial', data= train)
summary(results) 

#fit logistic model with proposed variables
results2<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides, family= 'binomial', data= train)
summary(results2) 

#check to see if reduced (without fixed acidity and residual sugar) model better
#can a subset of predictors be dropped
#df = 2 because looking to remove 2 predictors
#H0: B for fixed acid = B for resid sugar = 0 (remove them- use reduced model)
#HA: at least one B != 0 (don't remove, full model)
1-pchisq(results2$deviance-results$deviance, 2)
#p=0.95, p> 0.05, fail to reject the null, go with reduced model


#fit logistic model without fixed acidity and residual sugar 
results.nocitric<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides, family= 'binomial', data= train)
summary(results.nocitric) 
#wald test-> all p values significant
confint(results.nocitric, level= 0.95)

#check VIFs 
library(faraway)
vif(train[, c(11,8,2,5)])


#call library to use ROCR
library(ROCR)

#set up for roc (false positive rate on x axis, true postive rate on y axis)
preds<- predict(results.nocitric, newdata=test, type='response')
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
#AUC = 0.8017135 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 


#low is failure, Qhigh is sucess 
#confusion matrix
table(test$quality.binary, preds>0.5)
#overall error rate: 128 + 474 / 2501 + 128 + 474 + 146 = 0.185
#False Positive Rate: 128 / 2501 + 128 = 128/2629 = 0.0467
#False Negative Rate: 474 /474 + 146 = 474/620 = 0.7645
#Sensitivity: 146 / 474+146 = 0.235 
#Specificity:   2501/2501 +128 = 2501/2629 = 0.995

#looking at distribution of predictions
hist(preds)

table(test$quality.binary, preds>0.4)
#overall error rate: 375 + 245 / 2361 + 268 + 375 + 245 = 0.19
#False Positive Rate: 268 / (2361+268) = 128/2629 = 0.1
#False Negative Rate: 375 /(375+245) = 0.6
#Sensitivity: 245 /(375+245) = 0.395 
#Specificity:   2361 / (2361+268) = 0.898

table(test$quality.binary, preds>0.65)
#overall error rate:(582 + 38) / (2597 + 32 + 582 + 38) = 0.19
#False Positive Rate: 32 / (2597+32) = 128/2629 = 0.012
#False Negative Rate: 582 /(582+38) = 0.939
#Sensitivity: 38 /(582+38) = 0.061
#Specificity:   2597 / (2597+32) = 0.988

#======================= Auto's ==============================================

#automates search using glm 

#intercept only model (starting point)
regnull<-glm(quality.binary~1, family = 'binomial' ,data=train)
#look at summary to be sure it worked
summary(regnull)


#full model  (all the regressors) (ending point)
regfull<- glm(quality.binary~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + 
               chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + 
               sulphates + alcohol + color_of_wine, family = 'binomial', data= train)    
#list all predictors to exclude quality (non binary)
#looked at summary to be sure it worked
summary(regfull)
?step
#autosearch procredures
#forward: starts w/ int only model  and adds lowest AIC??
step(regnull, scope= list(lower=regnull, upper=regfull), direction='forward')
#kept alcohol, volatile acidity, sulphates, residual sugar, free sulfur,
#total sulfur, chlorides, ph, fixed acidity, density and color

#backwards: starts w/ full model and eliminates highest AIC???
step(regfull, scope=list(lower=regnull, upper=regfull), direction='backward')
# kept same as forward

#stepwise
step(regnull, scope=list(lower=regnull, upper=regfull), direction='both')
#kept same as other two
#kept alcohol, volatile acidity, sulphates, residual sugar, free sulfur,
#total sulfur, chlorides, ph, fixed acidity, density and color

#can also specify starting position of stepwise more directly
step(results.nocitric, scope=list(lower=regnull, upper=regfull), direction='both' )
#same as other methods

auto<- glm(quality.binary~alcohol + volatile.acidity + sulphates+ residual.sugar +
             free.sulfur.dioxide +total.sulfur.dioxide+chlorides+pH + fixed.acidity+
             density+color_of_wine, family= 'binomial', data=train)

summary(auto)
summary(results.nocitric)


#check to see if can drop subset (since results.nocitric is a subset of auto)
#test to see if can remove additional predictors (age & sex)
#df = 12-5 = 7 becasue looking to remove 2 predictors
#H0: B for sulphates, resid sugar, free sulfur, total sulfur, pH fixed acidty and color = 0 (remove them- use reduced model)
#HA: at least one B != 0 (don't remove, full model)
1-pchisq(results.nocitric$deviance-auto$deviance, 7)
#p=3.99e-13, p< 0.0.5, reject the null, go with full model

#confidence interval: none of them contain zero...
confint(auto, level= 0.95)



#none of the p-values in auto show can be removed-> check VIFS for multicollinearity?
library(faraway)
#look at vifs for auto first?
vif(train[, c(1,2,4,5,6,7,8,9,10,11)]) 

#highest VIF are most representative of the other predictors
#density 17.48
#fixed.acidity = 5.64
#residual sugar = 7.46
#alcohol = 5.36


library(ROCR)
#set up for roc (false positive rate on x axis, true postive rate on y axis)
preds<- predict(auto, newdata=test, type='response')
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
#AUC = 0.81367 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 


#low is failure, Qhigh is sucess 
#confusion matrix
table(test$quality.binary, preds>0.5)
table(test$quality.binary, preds>0.6)
# more concerned with false positive than false negative
# threshold 0.5
# overall error rate: (131 + 446) / (2498 + 131 + 446 + 174) = 17.8%
# False Positive Rate: 131 / (2498 + 131) = 5.0%
# False Negative Rate: 446 /(446 + 174) = 71.9%
# Sensitivity: 174/(446+174) = 28.1%
# Specificity:   2501/(2498 + 131) = 95.1%

# threshold 0.6
# overall error rate: (41 + 515) / (2498 + 131 + 446 + 174) = 17.1%
# False Positive Rate: 41 / (2498 + 131) = 1.6%
# False Negative Rate: 515 /(446 + 174) = 83.1%
# Sensitivity: 105/(446+174) = 16.9%
# Specificity:   2588/(2498 + 131) = 98.4%

#not much difference
n<-length(train)
p<-5

COOKS<-cooks.distance(results2)
COOKS[COOKS>qf(0.5,p,n-p)]

p<-12

COOKS<-cooks.distance(auto)
COOKS[COOKS>qf(0.5,p,n-p)]