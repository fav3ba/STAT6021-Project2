#STAT 6021 
#Project 2

library(tidyverse)

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

##have R treat quality as categorical
#moved this segment to before the data split
wines$quality<-factor(wines$quality) 
is.factor(wines$quality)

##check coding scheme
contrasts(wines$quality)
levels(wines$quality)

##collapse 3-6 -> bad, 7-9 -> good
new.levels<-c("Low", "Low", "Low", "Low", "QHigh", "QHigh", "QHigh") ##need to match up with the order
new.levels
wines$quality.binary<-factor(new.levels[wines$quality]) ##add this new binary variable to data frame
is.factor(wines$quality.binary)
contrasts(wines$quality.binary)
 # low is now the reference class
##add this new binary variable to data frame
#low is the reference class - I used Qhigh so that Low would be reference- since
#high quality should be seen as success- easier to interpret later

# splits all wine data into training and testing, 50% train, 50% test
# good idea to set.seed for now so we are all on same page with data
set.seed(69) #for if we want the split to be the same each time
train_test_split<-sample.int(nrow(wines), floor(.5*nrow(wines)), replace = F)
train<-wines[train_test_split, ]
test<-wines[-train_test_split, ]

nrow(train[train$quality.binary=="QHigh",]) #657 in the training
nrow(test[test$quality.binary=="QHigh",]) #620 in the test

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
nrow(train[train$color_of_wine ==1,])  #2429 whites

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


#be sure there are 
nrow(train[train$quality.binary=="QHigh",]) #657 in the training
nrow(test[test$quality.binary=="QHigh",]) #657 in the test

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



#fit logistic model with proposed variables
results<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides+citric.acid, family= 'binomial', data= train)
summary(results) 

#is the model useful?
#check to see if B's = 0 
#testing all coefficients, use Delta G^2 test statistic
#df = 3247(df of null deviance) - 3242(res deviance for full) = 5
#H0: all Bs  = 0 (not useful)
#HA: at least one B non zero (useful)
#test stat delta G^2 = null deviance - residual devaince
results$null.deviance-results$deviance #3271.0-2668.1
#602.96
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
#z-stat: -0.641
#p-value: 0.521346
#p>0.05, fail to reject the null
#citric acid not useful with other predictors in the model

#fit logistic model with proposed variables
results.nocitric<-glm(quality.binary~alcohol+density+volatile.acidity+chlorides, family= 'binomial', data= train)
summary(results.nocitric) 

#call library to use ROCR
library(ROCR)

#set up for roc (false positive rate on x axis, true positive rate on y axis)
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
#overall error rate: 128 + 474 / 2501 + 128 + 474 + 146 =
#False Positive Rate: 128 / 2501 + 128 = 128/2629 = 
#False Negative Rate: 474 /474 + 146 = 474/620 = 
#Sensitivity: 146 / 474+146 =  =
#Specificity:   2501/2501 +128 = 2501/2629 = 



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

step.model<- glm(quality.binary~alcohol + volatile.acidity + sulphates + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + chlorides+pH + fixed.acidity + density + color_of_wine, family= 'binomial', data=train)

step(step.model, scope=list(lower=regnull, upper=step.model), direction='backward')

#can also specify starting position of stepwise more directly
step(results.nocitric, scope=list(lower=regnull, upper=regfull), direction='both' )
#same as other methods

auto<- glm(quality.binary~alcohol + volatile.acidity + sulphates+ residual.sugar + free.sulfur.dioxide +total.sulfur.dioxide+chlorides+pH + fixed.acidity + density+color_of_wine, family= 'binomial', data=train)

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

#non of the p-values in auto show can be removed-> check VIFS for multicollinearity?
library(faraway)
#Do the same thing- not sure why including wine color causes problems
#vif(train[, c(1,2,3,4,5,6,7,8,9,10,11)]) 
vif(train[-c(12:14)])
#highest VIF are most representative of the other predictors
#density 17.48
#fixed.acidity = 5.64
#residual sugar = 7.46
#alcohol = 5.36

#fit model with just those and use it as starting point for auto selection?
vifResults <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol, family = 'binomial', data = train)
summary(vifResults)


#can also specify starting position of stepwise more directly
step(vifResults, scope=list(lower=regnull, upper=regfull), direction='both' )
#same as other methods


#is auto selection model useful? yes it is
auto$null.deviance-auto$deviance
#df = 11
1-pchisq(auto$null.deviance-auto$deviance,11)
#p-value 0 -> reject null -> this model is useful


#Need to deal with VIF's 

#below is just me checking out the auto model
summary(auto)
#call library to use ROCR
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
#not much difference

#checking out the VIF model
summary(vifResults)
#call library to use ROCR
library(ROCR)

#set up for roc (false positive rate on x axis, true postive rate on y axis)
preds<- predict(vifResults, newdata=test, type='response')
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
#AUC = 0.7831038 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 

#add next highest vif -> totalsulfur
vifResults_totalsulfur <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+total.sulfur.dioxide, family = 'binomial', data = train)
summary(vifResults_totalsulfur)

#Wald test
#H0: B for total sulfur = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 0.849569 - fail to reject the null
#in the presence of other predictors, not useful drop total sulfur


#add next highest vif -> pH
#fit model with just those and use it as starting point for auto selection?
vifResults_pH <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH, family = 'binomial', data = train)
summary(vifResults_pH)

#Wald test
#H0: B for pH = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 1.9e-8 - reject the null
#in the presence of other predictors, useful keep pH in model

#add next highest vif -> free sulfur
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsul <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide, family = 'binomial', data = train)
summary(vifResults_pHsul)

#Wald test
#H0: B for free sulfur = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 9.97e-5 - reject the null
#in the presence of other predictors, useful keep free sulfur in model

#add next highest vif -> volatile acidity
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsulva <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity, family = 'binomial', data = train)
summary(vifResults_pHsulva)

#Wald test
#H0: B for volatile acidity = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 1.77e-15 - reject the null
#in the presence of other predictors, useful keep voltile acidity in model

#pause and see if subtantial increas in AUC
#call library to use ROCR
library(ROCR)

#set up for roc (false positive rate on x axis, true postive rate on y axis)
preds<- predict(vifResults_pHsulva, newdata=test, type='response')
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
#AUC = 0.802508 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 
# at this point only increased about 2% is the complexity really worth it???


#add next highest vif -> citric acid
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsulvaca <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity +citric.acid, family = 'binomial', data = train)
summary(vifResults_pHsulvaca)

#Wald test
#H0: B for citric acid = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 0.51933 - fail to reject the null
#in the presence of other predictors, not useful - remove citric acid from model

#add next highest vif -> sulphates
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsulvas <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity+sulphates, family = 'binomial', data = train)
summary(vifResults_pHsulvas)

#Wald test
#H0: B for sulphates = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 3.71e-8 - reject the null
#in the presence of other predictors, useful keep sulphates in model

#add next highest vif -> chlorides
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsulvasc <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity+sulphates+chlorides, family = 'binomial', data = train)
summary(vifResults_pHsulvasc)

#Wald test
#H0: B for chlorides = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 0.15861 - fail to reject the null
#in the presence of other predictors, not useful remove chlorides from model

#add next highest vif -> color
#fit model with just those and use it as starting point for auto selection?
vifResults_pHsulvascw <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity+sulphates+color_of_wine, family = 'binomial', data = train)
summary(vifResults_pHsulvascw)

#Wald test
#H0: B for color_of_wine = 0 (remove not useful)
#HA: B != 0 (keep - useful)
#p-value: 0.000355 - reject the null
#in the presence of other predictors, useful keep color of wine in model


#renaming to make it easier
vifResults_final <- glm(quality.binary~density + fixed.acidity + residual.sugar + alcohol+pH + free.sulfur.dioxide +volatile.acidity+sulphates+color_of_wine, family = 'binomial', data = train)
summary(vifResults_final)

#pause and see if substantial increase in AUC
#call library to use ROCR
#library(ROCR)

#set up for roc (false positive rate on x axis, true postive rate on y axis)
preds<- predict(vifResults_final, newdata=test, type='response')
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
#AUC = 0.8123658 - since this value is greater than 0.5 this validates that the 
#model performs better than randomly guessing. 
# at this point only increased about 3% is the complexity really worth it???



