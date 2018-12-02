

library("glmulti")
library("metafor")

library("ggplot2")
library("gridExtra")
library("lubridate")
library("dplyr")
library("plyr")


networkomisegoTX <- read.csv("C:/Users/ujjwa/Desktop/Home_Work/Stat/Project/Our token/networkomisegoTX.txt", sep="")
#load Library 
library("sqldf", lib.loc="~/R/win-library/3.5")
#filter all transcation above the limit
tokenAmt = 492480511000000000000000000


networkomisegoTX_clean <-sqldf("select * from networkomisegoTX where tokenAmount<'$tokenAmt' order by tokenAmount desc")

networkomisegoPRI <- read.csv("C:/Users/ujjwa/Desktop/Home_Work/Stat/Project/Our token/OMISEGO", sep="")

### Studying distribution of the opening price . 

timePrices <- subset(networkomisegoPRI, select=c("Date","Open", "Close"))
timePrices$Date <- mdy(timePrices$Date)
timePrices <- unique(timePrices)
#summary(timePrices)
plot(timePrices$Date, timePrices$Open, main = "Opening prices VS date", xlab = "Date", ylab="Open price")


### Studying the distribution of number of unique buyers each day. 
timeBuyFreq<-sqldf("select count(distinct toNodeID) as count, round(unixTime/(1440 * 60)) as DATE from networkomisegoTX_clean group by round(unixTime/(1440* 60)) order by unixTime")
timeBuyFreq$DATE <- as.Date(as.Date(as.POSIXct(timeBuyFreq$DATE*(1440*60), origin="1970-01-01")), "%Y-%m-%d")
timeBuyFreq <- unique(timeBuyFreq)
#summary(timeBuyFreq)
outliers <- boxplot(timeBuyFreq$count, main="Unique buyer count distribution", ylab="unique buyer count")$out



### Combine opening price and unique buyer count for each day
priceSellForEachDay <- merge(x=timePrices, y=timeBuyFreq, by.x=c("Date"), by.y = c("DATE"))
newSet <- priceSellForEachDay$count
maxCount = max(newSet[complete.cases(newSet)])
minCount = min(newSet[complete.cases(newSet)])
priceSellForEachDay <- subset(priceSellForEachDay, count<maxCount & count>minCount)
#head(priceSellForEachDay)
cor(priceSellForEachDay$Open, priceSellForEachDay$count, method=c("pearson", "kendall", "spearman"))



### Compare opening price and number of transactions for each day
timeBuyFreq1<-sqldf("select count(*) as count, round(unixTime/(1440 * 60)) as DATE from networkomisegoTX_clean group by round(unixTime/(1440* 60)) order by unixTime")
timeBuyFreq1$DATE <- as.Date(as.Date(as.POSIXct(timeBuyFreq1$DATE*(1440*60), origin="1970-01-01")), "%Y-%m-%d")
timeBuyFreq1 <- unique(timeBuyFreq1)
priceSellForEachDay1 <- merge(x=timePrices, y=timeBuyFreq1, by.x=c("Date"), by.y = c("DATE"))
newSet1 <- priceSellForEachDay1$count
maxCount1 = max(newSet1[complete.cases(newSet1)])
minCount1 = min(newSet1[complete.cases(newSet1)])
priceSellForEachDay1 <- subset(priceSellForEachDay1, count<maxCount1 & count>minCount1)
cor(priceSellForEachDay1$Open, priceSellForEachDay1$count, method=c("pearson", "kendall", "spearman"))



### Compare opening price and closing price of previous day for each day
y2=list()
y2 <- append(y2,0)
for(i in 2:nrow(priceSellForEachDay)){
  y2 <- append(y2,priceSellForEachDay$Close[i-1])
}
priceSellForEachDay$ClosePrev <- unlist(y2, use.names=FALSE)



### Merge all three regressors, and calculate price return, simple price return is given as  $$P_t ??? (P_t ??? 1)/P_t ??? 1$$
corDataSet <- merge(x= priceSellForEachDay, y=priceSellForEachDay1, by.x=c("Date"), by.y=c("Date"))


y=list()
for(i in 1:nrow(corDataSet)-1){
  y <- append(y,(corDataSet$Open.x[i]-corDataSet$Open.x[i+1]-corDataSet$Open.x[i+2]-corDataSet$Open.x[i+3])/
                corDataSet$Open.x[i+1]+corDataSet$Open.x[i+2]+corDataSet$Open.x[i+3])
}
y <- append(y,0)
corDataSet$priceReturn <- unlist(y, use.names=FALSE)

trainingRowIndex <- sample(1:nrow(corDataSet), 0.7*nrow(corDataSet))  # row indices for training data
trainingData <- corDataSet[trainingRowIndex, ]

### Building the prediction model
linearMod <- lm( priceReturn ~ poly(count.x,2) + poly(count.y,2) + 
                   poly(ClosePrev,2), data=trainingData) 
summary(linearMod)
AIC(linearMod)
plot(residuals(linearMod))


  # model training data
#######################
set.seed(999)  # setting seed to reproduce results of random sampling
testData  <- corDataSet[-trainingRowIndex, ]   # test data
distPred <- predict(linearMod, testData)
actuals_preds <- data.frame(cbind(actuals=testData$priceReturn, 
                                  predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
print(min_max_accuracy)

