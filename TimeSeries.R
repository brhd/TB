## TIME SERIES ANALYSIS OF STOP TB PARTNERSHIP NEWS DATA
########################################################


## TEXT FILES USED HERE CAN BE PROVIDED UPON REQUEST

##  THREE TYPES OF TIME SERIES ARE CREATED WITH: 


            ## UNWEIGHTED SCORES (PART 1), 
            ## WEIGHTED SCORES (BY DIVIDING EACH SCORE BY THE NUMBER OF ARTICLES PER YEAR) (PART 2),  
            ## WEIGHTED SCORES (BY DIVIDING EACH SCORE BY THE NUMBER OF ARTICLES PER YEAR) (PART 3), 
            
## SCORES ARE WEIGHTED BECAUSE THE NUMBER OF ARTICLES WRITTEN PER UNIT TIME WAS SIGNIFICANTLY VARIABLE 
## THEREBY AFFECTING THE OVERALL TREND

## MONTHLY WEIGHTED SCORES (PART 3) ARE LIKELY TO BE THE MOST LOGICAL  BECAUSE SCORES HAVE BEEN AGGREGATED 
## AND REPRESENTED AS MONTHLY TOTALS WHILE CREATING THE TIME SERIES 



## Here we go:: 

install.packages("xts")
install.packages("ggplot2")
install.packages("qdap")

library(xts)
library(ggplot2)
library(reshape2)
library(stringr)
library(stringi)
library(qdap)


## reading the file


tbd <- read.csv("/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text_Full_Coded.csv", 
                header = T, stringsAsFactors = F, as.is = T)


## DOING A WORD COUNT

TS_CN <- paste0(tbd$Text, sep = " ", collapse = " ")

TS_wordcount <- sapply(gregexpr("\\W+", TS_CN), length) + 1


## MAKING VARIABLE NAMES SHORTER

colnames(tbd)[8] <- "AAA"
colnames(tbd)[9] <- "RDT"

### FINDING THE AVERAGE LENGTH OF THE TEXT 


## CREATING A  WORKING COPY OF THE DATA FRAME

tbdW <- tbd[,c(-2,-3)]


## CHANGING THESE COLUMNS INTO INTEGERS


tbdW$DR.TB <- as.numeric(tbd1$DR.TB)
tbdW$Funding <- as.numeric(tbd1$Funding)

## DIVIDING THE DATE BY MONTH AND ATTACHING TO THE DATAFRAME

short.date = strftime(as.POSIXct(tbd1$Date, format = "%m/%d/%y"), format = "%b-%y")
short.date1 = strftime(as.POSIXct(tbd1$Date, format = "%m/%d/%y"), format = "%y-%m")

tbdW <- cbind(tbdW, short.date1)
 


## CREATING A NEW DATAFRAME AND CHANGING ALL THE NAs TO 0 IN THE DATAFRAME 

tbd1 <- tbdW

tbd1[is.na(tbd1)] <- 0



### Aggregating the data


stat.DRTB = aggregate(tbd1$DR.TB ~ short.date1, FUN = sum)
stat.Funding = aggregate(tbd1$Funding ~ short.date1, FUN = sum)
stat.TBHIV = aggregate(tbd1$TB.HIV ~ short.date1, FUN = sum)
stat.AAA = aggregate(tbd1$AAA ~ short.date1, FUN = sum)
stat.RDT = aggregate(tbd1$RDT ~ short.date1, FUN = sum)
stat.Others = aggregate(tbd1$Others ~ short.date1, FUN = sum)




## and now combining into a dataframe

TotalScores <- Reduce(function(x, y) merge(x, y, all=TRUE), list(stat.DRTB, stat.Funding, stat.TBHIV, 
                                                                 stat.AAA, stat.RDT, stat.Others))

TotalScores[is.na(TotalScores)] <- 0



## creating a time series

myts <- ts(TotalScores, start = c(2005, 2), frequency = 12)

## REMOVING THE SHORT DATES NAs

myts <- myts[,-1]



## MAKING THE VARIABLES MORE READABLE

colnames(myts) <- c("DRTB","Funding","TBHIV","Advocacy","RDT", "Others")

## SUMS OF SCORES

TS1_Sums <- sort(colSums(myts), decreasing = T)


### DECOMPOSING THE TIME SERIES



myts1 <- myts[,1]
fit1 <- stl(myts2, s.window = "period")

myts2 <- myts[,2]
fit2 <- stl(myts3, s.window = "period")

myts3 <- myts[,3]
fit3 <- stl(myts4, s.window = "period")

myts4 <- myts[,4]
fit4 <- stl(myts5, s.window = "period")

myts5 <- myts[,5]
fit5 <- stl(myts5, s.window = "period")

myts6 <- myts[,6]
fit6 <- stl(myts6, s.window = "period")





## Plotting 

plot(myts, col = c("red", "blue", "black", "green"), 
     main = "Time Series plots for all themes", xlab = "Year")



plot(fit1, xlim = c(2004, 2016), col = "red") 
plot(fit2, xlim = c(2004, 2016), col = "green")
plot(fit3, xlim = c(2004, 2016), col = "blue")
plot(fit4, xlim = c(2004, 2016), col = "black")
plot(fit5, xlim = c(2004, 2016), col = "blue")
plot(fit6, xlim = c(2004, 2016), col = "green")



###########################################   NOW FOR THE NORMALIZED SCORES AND TABLES
################ PART 2 ##########################
##########################################################


tbd2 <- tbdW

tbd2[is.na(tbd2)] <- 0

## counting the number of stories per year

yearT <- table(tbd2$Year)

## NOW NORMALIZING THE VALUES BY THE YEAR

## ATTACHING THE FILE

attach(tbd2)

## TURNING THE YEAR INTO A CHARACTER VARIABLE

Year <- as.character(Year)



## WEIGHTING THE SCORES BY DIVIDING BY THE NUMBER OF ARTICLES PER YEAR

tbd2$DR.TB <- tbd2$DR.TB/yearT[Year]
tbd2$Funding <- tbd2$Funding/yearT[Year]
tbd2$TB.HIV <- tbd2$TB.HIV/yearT[Year]
tbd2$AAA <- tbd2$AAA/yearT[Year]
tbd2$RDT <- tbd2$RDT/yearT[Year]
tbd2$Others <- tbd2$Others/yearT[Year]



## YEARLY WEIGHTED SCORES 


Nstat.DRTB = aggregate(tbd2$DR.TB ~ short.date1, FUN = sum)
Nstat.Funding = aggregate(tbd2$Funding ~ short.date1, FUN = sum)
Nstat.TBHIV = aggregate(tbd2$TB.HIV ~ short.date1, FUN = sum)
Nstat.AAA = aggregate(tbd2$AAA ~ short.date1, FUN = sum)
Nstat.RDT = aggregate(tbd2$RDT ~ short.date1, FUN = sum)
Nstat.Others = aggregate(tbd2$Others ~ short.date1, FUN = sum)



## CREATING THE WEIGHTED LIST

NTotalScores <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Nstat.DRTB, Nstat.Funding, Nstat.TBHIV, 
                                                                 Nstat.AAA, Nstat.RDT, Nstat.Others))


NmyNSts <-  ts(NTotalScores, start = c(2005, 2), frequency = 12)



## REMOVING THE SHORT.DATE1 COLUMN

NmyNSts <- NmyNSts[,-1]

### RENAMING COLUMN NAMES

colnames(NmyNSts) <- c("DRTB","Funding","TBHIV","Advocacy","RDT", "Others")

### NOW DECOMPOSING THE TIME SERIES

fitN1 <- stl(NmyNSts[,1], s.window = "period")
fitN2 <- stl(NmyNSts[,2], s.window = "period")
fitN3 <- stl(NmyNSts[,3], s.window = "period")
fitN4 <- stl(NmyNSts[,4], s.window = "period")
fitN5 <- stl(NmyNSts[,5], s.window = "period")
fitN6 <- stl(NmyNSts[,6], s.window = "period")


### PLOTTING THE PLOTS

plot(NmyNSts, col = c("red", "blue", "black", "green"), 
     main = "Time Series plots (weighted) for all themes", xlab = "Year", lwd = 5)

plot(fitN1, xlim = c(2004, 2016), col = "green") 
plot(fitN2, xlim = c(2004, 2016), col = "red") 
plot(fitN3, xlim = c(2004, 2016), col = "blue") 
plot(fitN4, xlim = c(2004, 2016), col = "black") 
plot(fitN5, xlim = c(2004, 2016), col = "yellow") 
plot(fitN6, xlim = c(2004, 2016), col = "blue") 





#########################3333####################################
#### PART 3 
#################################################################

detach(tbd2)

## NOW DOING THE SAME THING BY DIVIDING BY MONTH

## TABLE OF NUMBER OF STORIES PER MONTH

monthT <- table(short.date1)


##  A NEW DATA FRAME TO WORK ON

tbd3 <- tbdW


## CHANGING THE NAME OF THE SHORT.DATE1 COLUMN TO AVOID CONFUSION 

colnames(tbd3)[9] <- "shortD"


## CHANGING NAS TO 0

tbd3[is.na(tbd3)] <- 0

## ATTACHING TBD3 AT POSITION 2

attach(tbd3)


## WEIGHTING THE SCORES BY DIVIDING BY THE NUMBER OF ARTICLES PER YEAR

tbd3$DR.TB <- tbd3$DR.TB/monthT[shortD]
tbd3$Funding <- tbd3$Funding/monthT[shortD]
tbd3$TB.HIV <- tbd3$TB.HIV/monthT[shortD]
tbd3$AAA <- tbd3$AAA/monthT[shortD]
tbd3$RDT <- tbd3$RDT/monthT[shortD]
tbd3$Others <- tbd3$Others/monthT[shortD]



## WEIGHTED SCORES values


Mstat.DRTB = aggregate(tbd3$DR.TB ~ short.date1, FUN = sum)
Mstat.Funding = aggregate(tbd3$Funding ~ short.date1, FUN = sum)
Mstat.TBHIV = aggregate(tbd3$TB.HIV ~ short.date1, FUN = sum)
Mstat.AAA = aggregate(tbd3$AAA ~ short.date1, FUN = sum)
Mstat.RDT = aggregate(tbd3$RDT ~ short.date1, FUN = sum)
Mstat.Others = aggregate(tbd3$Others ~ short.date1, FUN = sum)



## CREATING THE WEIGHTED LIST

MTotalScores <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Mstat.DRTB, Mstat.Funding, Mstat.TBHIV, 
                                                                  Mstat.AAA, Mstat.RDT, Mstat.Others))
class(MTotalScores)

head(MTotalScores)

Mmyts <-  ts(MTotalScores, start = c(2005, 2), frequency = 12)



## REMOVING THE SHORT DATE COLUMN BECAUSE IT HAS NAs, AND IS NOT REQUIRED

Mmyts <- Mmyts[,-1]


## SUMMING UP
TS3_Sums <- sort(colSums(Mmyts), decreasing = T)


colnames(Mmyts) <- c("DRTB","Funding","TBHIV","Advocacy","RDT", "Others")

### NOW DECOMPOSING THE TIME SERIES

fitM1 <- stl(Mmyts[,1], s.window = "period")
fitM2 <- stl(Mmyts[,2], s.window = "period")
fitM3 <- stl(Mmyts[,3], s.window = "period")
fitM4 <- stl(Mmyts[,4], s.window = "period")
fitM5 <- stl(Mmyts[,5], s.window = "period")
fitM6 <- stl(Mmyts[,6], s.window = "period")


### PLOTTING THE PLOTS

plot(Mmyts, col = "red", 
     main = "Time Series plots (weighted) for all themes", xlab = "Year", lwd = 5)

plot(fitM1, xlim = c(2004, 2016), col = "black") 
plot(fitM2, xlim = c(2004, 2016), col = "green") 
plot(fitM3, xlim = c(2004, 2016), col = "red") 
plot(fitM4, xlim = c(2004, 2016), col = "blue") 
plot(fitM5, xlim = c(2004, 2016), col = "yellow") 
plot(fitM6, xlim = c(2004, 2016), col = 101) 



