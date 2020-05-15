library(tidyverse) 
library(dplyr)
library(skimr)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(SnowballC)
library("tibble")
library(ggplot2)
library(mltools)
library(lmtest)
library(wordcloud)
library(gridExtra)
library('DMwR')
data_all<-read.csv('winemag-data-130k-v2.csv')
skim(data_all)

#remove na rows
data<-droplevels(na.omit(data_all))

#check top and bottom n countries by count
cat_total <- unlist(data$country)
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
head(cat_names_sort, n = 10)
tail(cat_names_sort, n = 10)

#check top and bottom n varieties by count
cat_total <- unlist(data$variety)
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
head(cat_names_sort, n = 10)
#tail(cat_names_sort, n = 10)


#histogram of points
ggplot(data.frame(data)) + stat_bin(aes(data$points),binwidth =1)

#histogram of price
quantile(data$price,na.rm=1,probs = seq(0,1,.01))
d<-data[data$price<100,]
ggplot(data.frame(d)) + stat_bin(aes(d$price),binwidth =5)

# plot to show points as well as prices against each country
p<-ggplot(data) + geom_boxplot(aes(x = reorder(country, price,median), points, 
                                fill = reorder(country, price, median)), show.legend=FALSE)
p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

#text review wordcloud
#first, build corpus an clean text
dfCorpus <- SimpleCorpus(VectorSource(sample_n(data, 20000)$description))
#View(dfCorpus)

# 1. Stripping any extra white space:
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
# 2. Transforming everything to lowercase
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
# 3. Removing numbers 
dfCorpus <- tm_map(dfCorpus, removeNumbers)
# 4. Removing punctuation
dfCorpus <- tm_map(dfCorpus, removePunctuation)
# 5. Removing stop words
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))

#stemming
dfCorpus <- tm_map(dfCorpus, stemDocument)
#update.packages("tm",  checkBuilt = TRUE)

data$description<-data.frame(text = sapply(dfCorpus, as.character), stringsAsFactors = FALSE)$text

DTM <- DocumentTermMatrix(dfCorpus)
View(DTM)
inspect(DTM)

#wordcloud
sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
head <- sums[1:500,]
par(mfrow=c(1,1))
wordcloud(words = head$term, freq = head$count, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
  