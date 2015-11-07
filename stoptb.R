## THE FIRST PART USES THE R PACKAGE RVEST TO SCRAPE THROUGH  http://stoptb.org/news/stories/
## AND SAVES TO A LOCAL CSV FILE AFTER CLEANING THE TEXT

## PART TWO CREATES A TEST SET, TRAINS AN ALGORITHM AND TESTS THE ALGORITHM ON THE TEST DATA SET


## installing required packages


install.packages("rvest")
install.packages("magrittr")
install.packages("selectr")
install.packages("stringr")
install.packages("RTextTools")
           
        
                
library(devtools)
library(rvest)
library(magrittr)
library(selectr)
library(stringr)
library(RTextTools)
library(tm)

install_github("brhd/RTextTools")

## now generating the list of yearly links


## creating the years variable

year <- 2005:2015
year1 <- c(2005, 2009:2015)
year2 <- 2006:2008


## loop to get the links for the years the second list

link.list1 <- character()
for (i in 1:length(year1)) {
  link.list1[i] <- paste("http://stoptb.org/news/stories/", year1[i], "/default.asp",  sep = "")
}
length(link.list1)

## loop to get the links for the years the second list

link.list2 <- character()
for (i in 1:length(year2)) {
  link.list2[i] <- paste("http://stoptb.org/news/stories/", year2[i], "/default.asp",  sep = "")
}
length(link.list2)



## now getting the weblinks for each year

## main page refers to the main page for each year

## this is the list of selector gadget urls: 

## 2005 .event a
## 2006 .subBoxInterior p , h6
## 2007 .subBoxInterior p , h6
## 2008 .subBoxInterior p , h6
## 2009 .event a
## 2010 .event a
## 2011 .event a
## 2012 .event a
## 2013 .event a
## 2014 .event a
## 2015 .event a




## initializing main.page

main.page <- NA

link.list.ins1 <- data.frame(links = NA,
                            urls = NA,
                            stringsAsFactors = F)


link.list.ins2 <- data.frame(links = NA,
                             urls = NA,
                             stringsAsFactors = F)

## looping for the first set: 



### this is the linktable function for the first type of page

listoflinks <- list()
links <- character()
urls1 <- character() 
for (i in seq(length(link.list1))){
  main.page <- html(link.list1[i]) 

  urls <- main.page %>% html_nodes(".event a") %>% html_attr("href") 

  urls1 <- paste("http://stoptb.org", urls, sep ="")
  links <- main.page %>% html_nodes(".event a") %>% html_text()

 
  listoflinks[[i]] <- data.frame(headline = links, URL = urls1, year = year1[i], stringsAsFactors = F)
  
}

## changing the list into a dataframe


DFoflinks <- do.call(rbind.data.frame, listoflinks)

## correcting the 155th URL
DFoflinks$URL[155] <- "http://www.un.org/News/Press/docs/2010/sgsm12791.doc.htm"




## this is the second function for the rest of the group


## forloop untested
## may not work
ListofText2 <- list()
for (i in 1:3)  {
  
  main.page <- html(link.list2[i]) 
  
  title <- main.page %>% html_nodes("h6") %>% html_text() 
  text1 <- main.page %>% html_nodes(".subBoxInterior p") %>% html_text() 


## a vector of dates
  dates5 <- str_match(text1, pattern = "([0-3]{1})?(-?)[0-9]{1}([0-9]{1})? \\w*[,]? 20[0-1]{1}[0-9]{1} -+ ")
  dates4 <- dates5[,1]
  dates3 <- dates4[!is.na(dates4)]
  dates2 <- gsub(" -+ ", "", dates3)
  dates1 <- gsub("\\d+-\\d", "\\d+", dates2)
  dates1 <- gsub("-", "", dates1)
  dates1 <- gsub(" ", "", dates1)
  dates <- as.Date(dates1,  format = "%d%B%Y")



## splitting the data 
## concatenating the vector into one

text2 <- paste(text1, collapse = " ")
  text  <- strsplit(text2, split = "([0-3]{1})?(-?)[0-9]{1}([0-9]{1})? \\w*[,]? 20[0-1]{1}[0-9]{1} -+ ")
## unlist the list
  textF <- unlist(text)
## first string is null, remove

  textF <- textF[-1]

## make sure the text and date lengths are equal, else merge leftover text in last date, 
## or remove extra dates

if (length(textF) != length(dates)) {
  if ((length(textF) > length(dates))) {
    textF[length(dates)] = paste(textF[length(dates):length(textF)], collapse = "")
     textF <- textF[1:length(dates)]  
      } else {
      dates <- dates[1:length(textF)] 
      }
}

## now making a dataframe out of the data, list type 2

ListofText2[[i]] <- data.frame(Date = dates, Title = title[1:length(dates)], 
                             Text = textF, Year = year2[i], stringsAsFactors = F) 
                                      
}

## changing the list into dataframe

DFofText2 <- do.call(rbind.data.frame, ListofText2)



### Three files created above: 
### Now to the more structured data: 



## the function to get the text from the article for the  DFoflinks

ListofText1 <- list()

for (i in seq(nrow(DFoflinks))) {

news <- html(DFoflinks$URL[i])

title <- news %>% html_nodes("h2") %>% html_text()
text <- news %>% html_nodes("p") %>% html_text()

##cutting out the last portion of the text

text1 <- text[1:length(text)-1]
text2 <-paste(text1, collapse = " ")

## processing the dates: 

date3 <- word(text2, c(1, 2, 3))
date2 <- paste(date3, collapse = "")
date1 <- gsub("-\\d+", "", date2)
date <- as.Date(date1,  format = "%d%B%Y")

## replacing the tabs 

text3 <- gsub("\"", "", text2) 
text4 <- gsub("\t", "", text3) 
text5 <-gsub("\\*", "", text4)
text6 <- word(text5, start = 5, end = -1)


textT <- str_replace_all(text6,"[^[:graph:]]", " ")

## now making the dataframe written in a list: 

ListofText1[[i]] <- data.frame(Date = date, Title = title, Text = textT, 
                               Year = DFoflinks$year[i], stringsAsFactors = F)

}


DFofText1 <- do.call(rbind.data.frame, ListofText1)



## writing down to csv files

write.csv(DFofText1, file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text1.csv", row.names = F)
write.csv(DFofText2, file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text2.csv", row.names = F)



#############
## PART 2
###############




### generating a 20% random selection of text from the 646 for supervised analysis

TextM <- read.csv(file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text_Merged.csv", header = TRUE)

testf <- sample(1:646, 130, replace = F )
sort(testf)

# Making a training dataset

## writing to a csv file

TrainS <- TextM[testf,]


## this is the output of testF

## [1]  92 327 420 508 404 597 419 499 343 526  28 180  63 168  89 457 278  59 330  98 173  77 235   7 614
## [26] 259 466 634 515 135  32 417 550  99 234 218 399  79 432 273 431  58 379 109 256 495 212 593 464 351
## [51] 426  24 616 551 251 428 191 439 429 459 503 238 521  38 281 456 209 228 383 511 141 425  69  71 247
## [76] 444  84 325 395 116 206 356 125 522 139 189  66 244 205 213 121 150 612 315  43  55 422 519 354 339
## [101] 304 564 375 560 591  41 494 541 523 600  80 393 217   8 562 197 227 333 181 119 372 538 533  90  93
## [126] 146 377 231 633 477
class(TrainS)

write.csv(TrainS, file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/TrainS.csv", row.names = F)

### 
### -- Now on to the supervised learning method using the RTextTools package::
###

## the trainSC file has articles broken down by individual themes. 

trainSet <- read.csv(file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/TrainSC.csv", 
                     header = TRUE, stringsAsFactors = F, encoding = "utf-8-mac")

### last obs has no data and NA values



trainSet <- trainSet[-155,]

# trainSet$Text <- str_replace_all(trainSet$Text, pattern = "[^[:alnum:]]", replacement = " ")
# trainSet <- as.data.frame(trainSet)
# str(trainSet)


DocMat <- create_matrix(textColumns = c(trainSet$Date, trainSet$Text, trainSet$ThemeN), language="english", 
                        minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, 
                        ngramLength=1, originalMatrix=NULL, removeNumbers=FALSE, 
                        removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  
                        stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

## can play around with the removeSparseTerms parameter


## creating a container

Cont <- create_container(DocMat, trainSet$ThemeN, trainSize = 1:154, virgin = FALSE)

## now creating a model

model <- train_model(Cont, "SVM", kernel = "linear", cost = 1)


### now reading the test data:

testD <- read.csv(file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text_Merged.csv",
                  header = TRUE, stringsAsFactors = F, encoding = "utf-8-mac")

## creating the prediction matrix

predMat <- create_matrix(textColumns = c(testD$Date, testD$Text), language="english", 
                         minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, 
                         ngramLength=1, originalMatrix = DocMat, removeNumbers=FALSE, 
                         removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  
                         stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)#, weighting = tm::weightTfIdf)

predCont <- create_container(predMat, labels=rep(0,646), 
                                        testSize=1:646, virgin=FALSE)

results <- classify_model(predCont, model)

table(results$SVM_LABEL) 

table(trainSet$ThemeN)

## new tables with coding only for advocacy
trainSet$ThemeA <- -1

for (i in 1:154) {
if (trainSet$ThemeN[i] == 5) {
    trainSet$ThemeA[i] = 1  
     } else if (trainSet$ThemeN[i] == 6) {
            trainSet$ThemeA[i] = 2
           }
}

table(trainSet$ThemeA)

for (i in 1:154) {
  if (trainSet$ThemeN[i] == 6) {
    trainSet$ThemeA[i] = 2  
  }
}

table(trainSet$ThemeN)

tail(trainSet,10)

### now using this new coding scheme only for advocacy

DocMat2 <- create_matrix(textColumns = c(trainSet$Date, trainSet$Text, trainSet$ThemeA), language="english", 
                        minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, 
                        ngramLength=1, originalMatrix=NULL, removeNumbers=FALSE, 
                        removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  
                        stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

## can play around with the removeSparseTerms parameter


## creating a container

Cont <- create_container(DocMat2, trainSet$ThemeA, trainSize = 1:154, virgin = FALSE)

## now creating a model

model <- train_model(Cont, "SVM", kernel = "linear", cost = 1)


### now reading the test data:

testD <- read.csv(file = "/Users/krp/Dropbox/Research/TB/ContAnal/Data/Text_Merged.csv",
                  header = TRUE, stringsAsFactors = F, encoding = "utf-8-mac")

## creating the prediction matrix

predMat <- create_matrix(textColumns = c(testD$Date, testD$Text), language="english", 
                         minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, 
                         ngramLength=1, #originalMatrix = DocMat, 
                         removeNumbers=FALSE, 
                         removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  
                         stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)#, weighting = tm::weightTfIdf)

predCont <- create_container(predMat, labels=rep(0,646), 
                             testSize=1:646, virgin=FALSE)


results <- classify_model(predCont, model)

table(results$SVM_LABEL)

fmat <- findFreqTerms(DocMat, lowfreq = 20)


mat <- as.matrix(DocMat2)
matfreq <- colSums(mat)
matfreq <- sort(matfreq, decreasing  = TRUE)
write.csv(head(matfreq, 100), file = "/Users/krp/Dropbox/Research/TB/Docs/matfreq100.txt")
write.csv(matfreq, file = "/Users/krp/Dropbox/Research/TB/Docs/matfreqAll.txt")


