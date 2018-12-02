#Reads the data from file location
networkomisegoTX <- read.csv("C:/Users/ujjwa/Desktop/Home_Work/Stat/Project/Our token/networkomisegoTX.txt", sep="")
#load Library 
library("sqldf", lib.loc="~/R/win-library/3.5")
#filter all transcation above the limit
tokenAmt = 492480511000000000000000000

networkomisegoTX_clean <-sqldf("select * from networkomisegoTX where tokenAmount<'$tokenAmt' order by tokenAmount desc")


Active_User_buying = sqldf("select toNodeId as node,count(*) as buy from networkomisegoTX_clean group by toNodeId")

Active_User_selling = sqldf("select romNodeId as node,count(*) as sell from networkomisegoTX_clean group by romNodeId")


activity <- merge(Active_User_buying, Active_User_selling, by="node", all.x=TRUE)

activity[is.na(activity)] <- 0

mostActive = sqldf(" select node from (select node,sum(buy+sell) as txn, buy/sell as ratio from activity where (ratio>0.5 AND ratio<2)  group by node order by txn desc) limit 1")

#Selling pattern

dat <- as.numeric(mostActive$node)



userA_data_Selling<-sqldf("select round(unixTime/(30* 60)) as unixTime,count(*) as sellingFreq from networkomisegoTX_clean where romNodeId=297278 group by round(unixTime/(30* 60)) order by unixTime")
#Buying Pattern
userA_data_Buying<-sqldf("select round(unixTime/(30* 60)) as unixTime,count(*) as buyingFreq from networkomisegoTX_clean where toNodeId=297278 group by round(unixTime/(30* 60)) order by unixTime")
#Selling plot
library("ggplot2")
library("gridExtra")


#sell_plot <- ggplot(data=userA_data_Selling, aes(x=userA_data_Selling$unixTime, y=userA_data_Selling$sellingFreq, group=1)) +geom_line()+ geom_point()
#buy_plot <- ggplot(data=userA_data_Buying, aes(x=userA_data_Buying$unixTime, y=userA_data_Buying$buyingFreq, group=1)) +geom_line()+ geom_point()




#Selling pattern
overall_data_Selling<-sqldf("select round(unixTime/(30* 60)) as unixTime,count(*) as sellingOverallFreq from networkomisegoTX_clean group by round(unixTime/(30* 60))  order by unixTime")
#Buying Pattern
overall_data_Buying<-sqldf("select round(unixTime/(30* 60)) as unixTime,count(*) as buyingOverallFreq from networkomisegoTX_clean group by round(unixTime/(30* 60))  order by unixTime")


sell_joint <- merge(overall_data_Selling, userA_data_Selling, by="unixTime", all.x=TRUE)

sell_joint[is.na(sell_joint)] <- 0

par(mfrow=c(2,1))

plot(sell_joint$unixTime, sell_joint$sellingOverallFreq, type = "b", frame = FALSE, pch = 19, 
          col = "red", xlab = "Time", ylab = "Selling Frequency",main = "Selling Distribution")
lines(sell_joint$unixTime, sell_joint$sellingFreq, type = "b", pch = 19, col = "blue",lty = 2)


legend("topleft", legend=c("Overall", "Top User"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


buy_joint <- merge(overall_data_Buying, userA_data_Buying, by="unixTime", all.x=TRUE)

buy_joint[is.na(buy_joint)] <- 0

plot(buy_joint$unixTime, buy_joint$buyingOverallFreq, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Time", ylab = "Buying Frequency",main = "Buying Distribution")
lines(buy_joint$unixTime, buy_joint$buyingFreq, type = "b", pch = 19, col = "blue",lty = 2)


legend("topleft", legend=c("Overall", "Top User"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

