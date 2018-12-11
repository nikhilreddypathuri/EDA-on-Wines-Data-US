library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(DT)
library(tm)
library(wordcloud)
library(RColorBrewer)
#library(SDMTools)
library(faraway)
library(useful)
library(coefplot)

#library(corrplot)
library(hexbin)
#library(lattice)
library(MASS)
library(car)
library(lattice)
library(hexbin)
library(grid)
library(gridExtra)

source('classDensity.r')
source('classEda.r')
source("panelCorrgram.r")


wines <- read_csv("winemag-data_first150k.csv")

#remove unnecesary columns
wines2 <- wines[,-c(4,8,9,11)]

#remove rows with NA
wines2 <- wines2[complete.cases(wines2), ]

#Determine country with most number of wineries
morewine <- wines2 %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(morewine, aes(x =reorder(country, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Countries with most Number of wineries') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#eda graph
windows()
classEda(wines2$points,
         lab1="Wine Points",
         units = "Points out of 100")

ggplot(data=wines2, aes(x=points))+
  geom_density(fill="cyan",color="black",adjust=2)+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#Determinig best wine producing countries
bestwine <- wines2 %>%
  filter(points >95) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

ggplot(bestwine, aes(x =reorder(country, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Premium Wine Producing Country - Point wise') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine bad wine producing countries
badwine <- wines2 %>%
  filter(points <85) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(badwine, aes(x =reorder(country, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Good Wines Producing Country - Point wise') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine countries with good averages in points
avgwinepoints<-wines2 %>%
  group_by(country)%>%
  summarize(avg_points = mean(points))%>% 
  arrange(desc(avg_points)) %>%
  head(10)

ggplot(avgwinepoints, aes(x =reorder(country, avg_points), y = avg_points )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Points', title = 'Countries with Best average points') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(75, 100))
  
#Determine countries that produce costliest wines
costlywines <- wines2 %>%
  filter(price >= 500) %>%
  arrange(desc(price)) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(costlywines, aes(x =reorder(country, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Countries that produce costly wine (Price > 5000$)') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine countries that produce economic wines
economywines<-wines2 %>%
  filter(price < 10) %>%
  arrange(price) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(economywines, aes(x =reorder(country, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Countries that produce Economic wine (Price < 100$)') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine Average price of wine Country Wise
avgwineprice<-wines2 %>%
  group_by(country)%>%
  summarize(avg_price = mean(price))%>% 
  arrange(desc(avg_price)) %>%
  head(10)

ggplot(avgwineprice, aes(x =reorder(country,avg_price), y =  avg_price )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Average Price of Wine (x10 USD)', title = 'Average price of wine - Country Wise') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine top 5 produced Varieties
morevariety <- wines2 %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(5)

ggplot(morevariety, aes(x =reorder(variety, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Variety', y = 'Count', title = 'Top 5 produced varieties') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determine top 5 Best rated varieties
bestvariety <- wines2 %>%
  filter(points >95) %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(bestvariety, aes(x =reorder(variety, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Variety', y = 'Count', title = 'Best Wine Varieties - Point wise') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Determining the amount of best varieties produced by different countries
wine_var_country <- wines2 %>%
  filter(points >95) %>%
  group_by(country, variety)

wine_var_country <-  wine_var_country[ wine_var_country$variety %in% bestvariety$variety, ]
wine_var_country$variety <- factor(wine_var_country$variety, levels = bestvariety$variety)


ggplot(wine_var_country, aes( x=country, fill=variety))+
  geom_bar(color=grey(.55)) +
  labs(x="Country",
       y="Count",
       title="Best varieties produced by different countries",
       fill="Variety") + 
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    values=c("red","orange",'yellow','green','pink','cyan','blue',"purple","brown","grey"),
    na.value="grey30") +
  scale_color_discrete(breaks=c("Pinot Noir","Cabernet Sauvignon","Chardonnay","Bordeaux-style Red Blend","Syrah","Red Blend","Merlot","Riesling","Champagne Blend","Nebbiolo"))

#Determining relationship between points and price
r<-1
i<-1
temp <- c(1,2,3,4,5)
while (r<137231) {
  if(wines2[r,5] > 1500)
  {
    temp[i]<-r
    i<-i+1
  }
  r<- r +1
}
wines3<- wines2[c(12541,31849,31851),]
ggplot(wines2,aes(x=points,y=price))+
  geom_point(shape=21, size=2, fill="green", color="black") +
  stat_smooth(color="purple",size=.8) +
  labs(x="Points",
       y="Price (x10 USD)",
       title="Points vs Price")+
  geom_vline(xintercept = 91)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(data=wines3,
             aes(label=country),
             nudge_y=120)+
  geom_label(data=wines3,
             aes(label=variety),
             nudge_y=270)+
  
  

cor(wines2$points, wines2$price, method = "spearman")
  

#Lets Go Deep into US data
#Filter out us data

uswine <- filter(wines2, wines2$country == "US")

makeWordCloud <- function(documents) {
  Encoding(documents) <- "UTF-8"
  documents <- iconv(documents, "UTF-8", "UTF-8",sub='')
  documents <- iconv(documents, 'UTF-8', 'ASCII')
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, content_transformer(gsub), pattern = "wine", replacement = " ")
  corpus = tm_map(corpus, content_transformer(gsub), pattern = "will", replacement = " ")
  corpus = tm_map(corpus, content_transformer(gsub), pattern = "made", replacement = " ")
  corpus = tm_map(corpus, stripWhitespace)
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,random.order=FALSE) 
}  

uswine <- uswine %>%
  filter(points > 95) %>%
  arrange()
  
  
makeWordCloud(uswine[["description"]][1:100])

#micromaps
uswine <- filter(wines2, wines2$country == "US")

microstates <- uswine %>%
  group_by(province) %>%
  summarise(n = n()) 
write.csv(usavgscore,file = "F:/MS/STAT 515/stat 515 project/wine_visualization/microstates.csv", row.names = FALSE)
usstates <-read.csv("microstates.csv")

#####
usavgscore <- uswine[,c("province","points","price")]
usavgscore <- aggregate(. ~ province, data=usavgscore, FUN=mean)
write.csv(spam2,file = "F:/MS/STAT 515/stat 515 project/wine_visualization/microstates.csv", row.names = FALSE)
booo <-read.csv("winedata2.csv")

#regression
usreg<- uswine[,c(4,5)]
mylog<- glm(original ~ ., data = testwines, family = binomial)
p<- predict(mylog,type = "response")
testdata<- usreg[,-c(1)]
val<- predict(mylog,testdata)
val<- as.data.frame(val)
max(val)
df <- usreg$variety
df2<- data.frame(df)

for (i in unique(df2$df)){
  df2[,paste0(i)]=ifelse(df2$df==i,1,0)
}
df2 <- df2[,-c(1)] 
usreg<-cbind(usreg,df2)
usreg <- usreg[,-c(2)]

predplot<- cbind(usreg[,c(1)],val)
colnames(predplot)[1]<- "original"
ggplot(predplot,aes(x=original,y=val))+
  geom_point() +
  geom_smooth(method=lm, color="blue",size=.5)
summary(predplot)
predplot<-as.data.frame(predplot)

r <-1
while(r<62140)
{
  if(predplot[r,2]>100)
  {
    predplot <- predplot[-c(r),]
  }
  r <- r+1
}
predplot <- predplot[complete.cases(predplot), ]
