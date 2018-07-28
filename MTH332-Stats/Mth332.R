library(moments)
setwd('Desktop')
#Importing Data
UnitedNationsData <- read.csv('UNdata.csv', header=TRUE,sep=",",col.names = c('Country','Year','Value','Other'))
UnitedNationsData$Other <- NULL
UnitedNationsData<-na.omit(UnitedNationsData)
head(UnitedNationsData)
Pakistan<- subset(UnitedNationsData, Country=='Pakistan')
Afghanistan<-subset(UnitedNationsData, Country=='Afghanistan')
year2015<- subset(UnitedNationsData, Year==2015)
year2000<- subset(UnitedNationsData, Year==2000)
hist(year2015$Value)
summary(year2015$Value)
summary(year2000$Value)
sd(year2015$Value)
kurtosis(year2015$Value)
skewness(year2015$Value)
qqnorm(year2015$Value)
qqline(year2015$Value)
hist(year2000$Value,col = 'Blue',xlab = 'Water Quality',main ='Histogram of Year 2000 and 2015 Water Quality' )
hist(year2015$Value, col = 'Red',add=TRUE)
legend(25, 95, legend=c("Year 2015", "Year 2000"),
       col=c("red", "blue"), lty=1)
box()

linModel<- lm(UnitedNationsData$Value~UnitedNationsData$Year)
plot(UnitedNationsData$Year,UnitedNationsData$Value,xlab = 'Year',ylab = 'Water Quality Value',
     main = 'Linear Regression Model for All Countries')
abline(linModel,col="red",lwd=2)
summary(linModel)

plot(Afghanistan$Year,Afghanistan$Value,xlab = 'Year',ylab = 'Water Quality Value',
     main = 'Linear Regression Model for Afghanistan')
linModel<- lm(Afghanistan$Value~Afghanistan$Year)
abline(linModel,col="red",lwd=2)
summary(linModel)


#Subsetting each country
Mexico<- subset(UnitedNationsData, Country=='Mexico')
Canada<- subset(UnitedNationsData, Country=='Canada')
USA<- subset(UnitedNationsData, Country=='United States')
# Summary Stats from Psych Package
describe(Mexico$Value)
describe(Canada$Value)
describe(USA$Value)
#Basic Stats on Each Country
hist(Mexico$Value)
hist(Canada$Value)
hist(USA$Value)

harborData<-read.csv('betterHarbor.csv',header=TRUE)
harborData$DO.Top<-as.numeric(harborData$DO.Top)
harborData$Do.Bot<-as.numeric(harborData$Do.Bot)
meanV<-mean(harborData$DO.Top)
hist(harborData$DO.Top)
CI(harborData$DO.Top,ci=.95)
CI(harborData$Do.Bot,ci=.95)

#Confidence Interval for Canada and Mexico Rmisc
CI(USA$Value,ci=.95)
CI(Canada$Value,ci=.95)
CI(Mexico$Value,ci=.95)


t.test(Canada$Value,Mexico$Value,paired = TRUE)
t.test(Canada$Value,USA$Value,paired = TRUE)
t.test(USA$Value,Mexico$Value,paired = TRUE)


difference=abs(mean(Canada$Value)-mean(Mexico$Value))
pooleddata<-c(Canada$Value,Mexico$Value)
dmeans <- numeric(100000) # vector to store means
for(i in 1:100000){
  g<-sample(52,26)
  dmeans[i]<-abs(mean(pooleddata[g])-mean(pooleddata[-g]))
}
tails <- which(dmeans >abs(mean(Canada$Value)-mean(Mexico$Value)))
length(tails)/100000

hist(dmeans, col = "gray")
abline(v=difference,col="red",lwd=2)


#Canada and Yemen (not similiar)
canVals<-justCanada$Value
mean(canVals)
yemVals<-justYemen$Value
mean(yemVals)
difference=abs(mean(yemVals)-mean(mexVals))
pooleddata<-c(yemVals,mexVals)
dmeans <- numeric(100000) # vector to store means
for(i in 1:100000){
  g<-sample(52,26)
  dmeans[i]<-abs(mean(pooleddata[g])-mean(pooleddata[-g]))
}
hist(dmeans, col = "gray")
abline(v=difference,col="red",lwd=2)

tails <- which(dmeans >abs(mean(canVals)-mean(yemVals)))
length(tails)/100000
