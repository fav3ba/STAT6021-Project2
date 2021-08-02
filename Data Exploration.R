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


boxplot(wines$quality~wines$color_of_wine, xlab='Color of Wine', ylab='Quality Rating', main='Quality Rating by Wine Color')

#new df (halfdata) that is from data indexed by rows of data, pick half
#(no replacement default)
#test_data<- wines[sample(nrow(wines), nrow(wines)*.1),]

