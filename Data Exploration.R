#STAT 6021 
#Project 2



#read in wehites data and add column with color of wine
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

#correlations doesn't work because of categorical variable
#bind variables of interest and then
#cor(wines)
