#read in data and add column with color of wine
whites<- read.csv('wineQualityWhites.csv', header=TRUE, sep=",")
whites$color_of_wine<- 'white'
summary(whites)
boxplot(whites$quality)

reds<-read.csv('wineQualityReds.csv', header= TRUE, sep=',')
reds$color_of_wine<- 'red'
summary(reds)
boxplot(reds$quality)


#merge them since have same variables
wines <- rbind(whites, reds)
head(wines)
tail(wines)
summary(wines)
boxplot(wines$color_of_wine,wines$quality)

#new df (halfdata) that is from data indexed by rows of data, pick half
#(no replacement default)
test_data<- wines[sample(nrow(wines), nrow(wines)*.1),]

