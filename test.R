library(readr)
library(tokenizers)
library(tm)
library(NLP)
library(SnowballC)
library(ROCR)

spam <- read_csv("F:/MS/CS 504/cs504 proj/spam.csv")
View(spam)

spam2 <- wines2[,c(4,3,1)]
r<-1
while(r<137231)
{
  if(spam2[r,1]<90)
  {
    spam2[r,3]<- "Good"
  }
  else
  {
    spam2[r,3]<- "Premium"
  }
  r<-r+1
}
spam2 <- spam2[,-c(1)]
spam2 <- spam2[,c(2,1)]
spam2 <- spam2[c(1:1500),]
View(spam2)

#convert UTF-8
Encoding(spam2$description)  <- "UTF-8"
goodtext <- iconv(spam2$description , "UTF-8", "UTF-8",sub='')
goodtext <- iconv(spam2$description , 'UTF-8', 'ASCII')
goodtext

corpus <- Corpus(VectorSource(goodtext))

writeLines(as.character(spam2$description[[30]]))

#eliminate colons brackets and hyphens
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " " , x))})
docs <- tm_map(corpus, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")

#remove punctuations
docs <- tm_map(docs, removePunctuation)

#writeLines(as.character(docs[[3]]))
#test <- as.character(docs[[3]])

#remove other non standard punctuations
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, " -")


#transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

#remove digits
docs <- tm_map(docs, removeNumbers)

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

#remove whitespaces
docs <- tm_map(docs, stripWhitespace)

#remove stemming
docs <- tm_map(docs,stemDocument)

#manually correct mistakes
docs <- tm_map(docs, content_transformer(gsub), pattern = "tc", replacement = " tc")
docs <- tm_map(docs, content_transformer(gsub), pattern = "tcs", replacement = "tc")
docs <- tm_map(docs, content_transformer(gsub), pattern = "questionstd", replacement = "question std")
docs <- tm_map(docs, stripWhitespace)

#create term document matrix
dtm <- DocumentTermMatrix(docs)
dtm
inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

#remove 3 lettered words
#dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20)))

#adding v1 column at start
DF <- data.frame(as.matrix(dtm), stringsAsFactors=FALSE)
DF2 <- data.frame(v1 = spam2$X1, DF)
View(DF2)

#column sum with unique v1
dtm2 <- aggregate(. ~ v1, data=DF2, FUN=sum)
View(dtm2)

#summing rows and columns
dtm3 <- dtm2[,c(-1)]
View(dtm3)

#freqdf <- data.frame(as.matrix(freq), stringsAsFactors = FALSE)

#dtm3 <- rbind(dtm2, data.frame(v1="Total",t(colSums(dtm2[,-1]))))
#dtm3 <- cbind(dtm3, Total = rowSums(dtm3[,-1]))
rownames(dtm3) <- dtm2[,1]

#finaldtm <- dtm3[,-1]
#rownames(finaldtm) <- dtm3[,1]
finaldtm <- as.data.frame(t(dtm3))

#count total ham and spam
phamspam <- aggregate(data.frame(count = spam2$X1), list(value = spam2$X1), length)


#math functions
`%/%` <- function(e1, e2) {return(e1 / e2)}
finaldtm <- within(finaldtm, pham <- Premium %/% phamspam$count[[1]])
finaldtm <- within(finaldtm, pspam <- Good %/% phamspam$count[[2]])

probham <- phamspam$count[[1]] / 1500
probspam <- phamspam$count[[2]] / 1500



#**************************
#test data naive bayes

testdata <- wines2[c(1501:2000),]
testdata <- wines2[,c(3)]


testdata <- as.data.frame(testdata[,c("description")])
colnames(testdata)<-"description"
View(testdata)

#convert UTF-8
Encoding(testdata$description) <- "UTF-8"
goodtestdata <- iconv(testdata$description, "UTF-8", "UTF-8",sub='')
goodtext <- iconv(testdata$description , 'UTF-8', 'ASCII')
goodtestdata

testcorpus <- Corpus(VectorSource(goodtext))

#eliminate colons brackets and hyphens
testdocs <- tm_map(testcorpus, toSpace, "-")
testdocs <- tm_map(testdocs, toSpace, ":")

#remove punctuations
testdocs <- tm_map(testdocs, removePunctuation)

#remove other non standard punctuations
testdocs <- tm_map(testdocs, toSpace, "’")
testdocs <- tm_map(testdocs, toSpace, "‘")
testdocs <- tm_map(testdocs, toSpace, " -")


#transform to lower case
testdocs <- tm_map(testdocs,content_transformer(tolower))

#remove digits
testdocs <- tm_map(testdocs, removeNumbers)

#remove stopwords
testdocs <- tm_map(testdocs, removeWords, stopwords("english"))

#remove whitespaces
testdocs <- tm_map(testdocs, stripWhitespace)

#remove stemming
testdocs <- tm_map(testdocs,stemDocument)

#manually correct mistakes
testdocs <- tm_map(testdocs, content_transformer(gsub), pattern = "tc", replacement = " tc")
testdocs <- tm_map(testdocs, content_transformer(gsub), pattern = "tcs", replacement = "tc")
testdocs <- tm_map(testdocs, content_transformer(gsub), pattern = "questionstd", replacement = "question std")
testdocs <- tm_map(testdocs, stripWhitespace)

#create term document matrix
testdtm <- DocumentTermMatrix(testdocs)
testdtm

#convert to data frame
testDF <- data.frame(as.matrix(testdtm), stringsAsFactors=FALSE)
View(testDF)

#remove new words

ncolumn <- ncol(testdtm)
nrows <- nrow(testdtm)

c <- 1
r <- 1
while(r<(nrows+1))
{
  while(c<(ncolumn+1))
  {
    cname <- colnames(testdtm)[c]
    if(is.na(finaldtm[cname,"pham"]))
    {
      testdtm <- testdtm[,-c(c)]
    }
    c <- c+1
  }
  c <- 1
  r <- r+1
}


#Predicting probability for each document to be ham and spam

ncolumn <- ncol(testDF)
nrows <- nrow(testDF)

c <- 1
r <- 1
product1 <- 1
product2 <- 1
prodham <- 1: nrows
prodspam <- 1: nrows

while(r<(nrows+1))
{
  while(c<(ncolumn+1))
  {
    cname <- colnames(testDF)[c]
    if(testDF[r,c] >= 1)
    {
      product1 <- product1 * finaldtm[cname,"pham"]
      product2 <- product2 * finaldtm[cname,"pspam"]
    }
    else
    {
      product1 <- product1 * (1-finaldtm[cname,"pham"])
      product2 <- product2 * (1-finaldtm[cname,"pspam"])
    }
    c <- c+1 
  }
  prodham[r] <- product1
  prodspam[r] <- product2
  product1 <- 1
  product2 <- 1
  c <- 1
  r <- r+1
}

#multiply total ham and spam probabilities

prodham <- prodham * probham
prodspam <- prodspam * probspam

result <- 1: nrows
c <- 1

while(c<(nrows+1))
{
  if(prodham[c] > prodspam[c])
  {
    result[c] <- "HAM"
  }
  else
  {
    result[c] <- "SPAM"
  }
  c <- c+1 
}

finalresult <- as.data.frame(result)
finalresult <- cbind(finalresult, Text = testdata$description)
View(finalresult)

#output result

write.csv(finalresult, "finalresult.csv", row.names=F)




tab <- table(testwines$predicted,testwines$original)
tab
accuracy <- sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)
tpr <- 59/(870+59)
1-tpr
fpr <- 42/(529+42)
pred<- prediction(p,testwines$original)
roc <- performance(pred,"tpr","fpr")
plot(roc)
abline(a=0,b=1)
legend(0.6,0.2,0.93, title = "AUC")
auc <- performance(pred, "auc")
auc <- unlist(slot(auc,y.values)
)
