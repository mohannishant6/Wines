#filtering data
data_q2_all<-droplevels(data[data$country=='US',])
data_q2<-droplevels(data_q2_all[c('description','province','region_1','taster_twitter_handle','variety','winery','price','points')])
skim(data_q2)

#convert text to corpus format
dfCorpus <- SimpleCorpus(VectorSource(data_q2$description))
# And lets see what we have
#View(dfCorpus)

#clean the text
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

#replace description with clean text
#data_q2$description<-data.frame(text = sapply(dfCorpus, as.character), stringsAsFactors = FALSE)$text

#function to model the data, perform backward elimiation and give summary stats
analyze_model<-function(df)
{
  model<-lm(points~.,df)
  print(summary(model))
  #predictions
  actuals_preds <- data.frame(cbind(actuals=df$points, predicteds=predict(model, subset(df,select=-c(points)))))  # make actuals_predicteds dataframe
  #evaluation metrics
  print(regr.eval(actuals_preds$actuals, actuals_preds$predicteds))
  #histogram of residuals
  hist(model$resid,main=" ",breaks=20,col="peachpuff",border="black",xlim = c(-10,10),ylim = c(0,10000))
  
  
  #backward elimination to identify best parameters
  step_AIC_backward <- step(model)
  print("BACKWARD ELIMINATION")
  model_back<-eval(step_AIC_backward$call)
  print(summary(model_back))
  #predictions
  actuals_preds <- data.frame(cbind(actuals=df$points, predicteds=predict(model_back, subset(df,select=-c(points)))))  # make actuals_predicteds dataframe
  #evaluation metrics
  print(regr.eval(actuals_preds$actuals, actuals_preds$predicteds))
  #histogram of residuals
  hist(model_back$resid,main=" ",breaks=20,col="peachpuff",border="black",xlim = c(-10,10),ylim = c(0,10000))
}

#Baseline: When mean is the prediction
actuals_preds <- data.frame(cbind(actuals=data_q2$points, predicteds=mean(data_q2$points)))  # make actuals_predicteds dataframe
#evaluation metrics
print(regr.eval(actuals_preds$actuals, actuals_preds$predicteds))


#Approach 1: extract sentiment: Method 1
sent2 <- get_nrc_sentiment(as.character(data_q2$description))
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")
ggplot(sent3, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment of Review") + theme(plot.title = element_text(hjust=0.5))

sent2$points<-data_q2$points
analyze_model(sent2)

#replace description with emotion score in the dataset
#data_q2<-subset(merge(data_q2,sent2,by=0),select=-c(description,Row.names))

#Approach 2: extract sentiment: Method 2
DTM <- DocumentTermMatrix(dfCorpus)
View(DTM)
inspect(DTM)
sent <- analyzeSentiment(DTM, language = "english")
sent1<-sent
sent1$points<-data_q2$points
analyze_model(sent1)

#Approach 3: Use original features other than text

#target encoding using category means for categorical features
target_encode<-function(df,col,weight=10)
{
  df<-na.omit(df)
  # get count and mean for each category
  counts<-setNames(aggregate(df$points,df[col],length),c(col,"target_count"))
  means<-setNames(aggregate(df$points,df[col],mean),c(col,"target_means"))
  #mapping to category of original data
  df<-left_join(left_join(df,counts),means)
  # calculation using population mean 
  df[col]<-(mean(df$points)*weight+(df$target_means*df$target_count))/(weight+df$target_count)
  #removing category mappings
  df<-subset(df, select = -c(target_count,target_means))
  
  return (df)
}
data_encoded<-subset(data_q2,select=c(region_1,province, taster_twitter_handle, winery,variety,points))
data_encoded<-target_encode(data_encoded,'region_1',10)
data_encoded<-target_encode(data_encoded,'province',10)
data_encoded<-target_encode(data_encoded,'taster_twitter_handle',10)
data_encoded<-target_encode(data_encoded,'winery',10)
data_encoded<-target_encode(data_encoded,'variety',10)


analyze_model(data_encoded)

#Approach 4: Take best features from all models
data_mix<-data.frame(points=data_q2$points,
                     sadness=sent2$sadness,surprise=sent2$surprise,positive=sent2$positive,
                     RatioUncertaintyLM=sent1$RatioUncertaintyLM,NegativityHE=sent1$NegativityHE,
                     winery=data_encoded$winery,taster_twitter_handle=data_encoded$taster_twitter_handle)
analyze_model(data_mix)
