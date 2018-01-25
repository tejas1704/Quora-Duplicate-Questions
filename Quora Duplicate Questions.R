#################################################
## 
## Quora Duplicate Questions
##
## Fall Semester - Final Project(Data Mining I)
##
##
## Authors: 1) Tejas Dhrangadharia
##          2) Karan Nisar
##          3) Sai Krishna Kanneti
##################################################

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)
library(tm)
library(syuzhet)
library(SnowballC)
library(randomForest)
library(wordcloud)

## Read the data
quora=read.csv('quora_duplicate_questions.csv',header = TRUE,sep=',')
quora<-quora[1:10000,]
set.seed(12345)

## Divide data into test and train
train.index = sample(1:nrow(quora), size = 0.7*nrow(quora))
train <- quora[train.index,]
test <- quora[-train.index,]
train$id <- as.numeric(train$ï..id)
train$qid1 <- as.numeric(train$qid1)
train$qid2 <- as.numeric(train$qid2)
train$question1 <- as.character(train$question1)
train$question2 <- as.character(train$question2)
train$is_duplicate <- as.numeric(train$is_duplicate)

test$id <- as.numeric(test$ï..id)
test$qid1 <- as.numeric(test$qid1)
test$qid2 <- as.numeric(test$qid2)
test$question1 <- as.character(test$question1)
test$question2 <- as.character(test$question2)
test$is_duplicate <- as.numeric(test$is_duplicate)

dim(train)
dim(test)

## Text Mining
docs1 <- Corpus(VectorSource(train$question1))
docs1 <- tm_map(docs1, removePunctuation) 
docs1 <- tm_map(docs1, removeNumbers) 
docs1 <- tm_map(docs1, tolower) 
docs1 <- tm_map(docs1, stemDocument) 
docs1 <- tm_map(docs1, stripWhitespace) 
docs1 <- tm_map(docs1, removeWords, stopwords("english"))
docs1 <- tm_map(docs1, removeWords, stopwords("SMART"))
doc = DocumentTermMatrix(docs1) 

a11 = doc$dimnames$Terms
freq <- colSums(as.matrix(doc))

head(table(freq), 20)
length(freq)

wf <- data.frame(word=names(freq), freq=freq) 
head(wf) 

p <- ggplot(subset(wf, freq>15), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 

set.seed(142) 
x11()
wordcloud(names(freq), freq, min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
dtms <- removeSparseTerms(doc, 0.99). 
dtms

library(cluster) 
d <- dist(t(dtms), method="euclidian") 
fit <- hclust(d=d, method="complete") 
plot(fit, hang=-1)
groups <- cutree(fit, k=6) 
rect.hclust(fit, k=6, border="red") 

library(fpc) 
d <- dist(t(dtms), method="euclidian") 
kfit <- kmeans(d, 2) 
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

docs2 <- Corpus(VectorSource(train$question2))
docs2 <- tm_map(docs2, removePunctuation) 
docs2 <- tm_map(docs2, removeNumbers) 
docs2 <- tm_map(docs2, tolower)
docs2 <- tm_map(docs2, stripWhitespace) 
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
docs2 <- tm_map(docs2, removeWords, stopwords("SMART"))
doc = DocumentTermMatrix(docs2) 
doc

freq <- colSums(as.matrix(doc))
head(table(freq), 20)
length(freq)
wf <- data.frame(word=names(freq), freq=freq) 
head(wf) 

p <- ggplot(subset(wf, freq>15), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 

set.seed(142) 
x11()
wordcloud(names(freq), freq, min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
dtms <- removeSparseTerms(doc, 0.99)
dtms

library(cluster) 
d <- dist(t(dtms), method="euclidian") 
fit <- hclust(d=d, method="complete") 
fit 
plot(fit, hang=-1)

groups <- cutree(fit, k=6) 
rect.hclust(fit, k=6, border="red")

library(fpc) 
d <- dist(t(dtms), method="euclidian") 
kfit <- kmeans(d, 2) 
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

## Text Cleaning Using Text mining
df = data.frame()
df.new = data.frame()

for (i in 1:nrow(train))
{
  docs1 <- Corpus(VectorSource(train$question1[i]))
  docs1 <- tm_map(docs1, removePunctuation) 
  docs1 <- tm_map(docs1, removeNumbers) 
  docs1 <- tm_map(docs1, tolower)
  docs1 <- tm_map(docs1, stemDocument) 
  docs1 <- tm_map(docs1, stripWhitespace) 
  docs1 <- tm_map(docs1, removeWords, stopwords("english")) 
  doc = TermDocumentMatrix(docs1) 
  a11 = doc$dimnames$Terms
  docs2 <- Corpus(VectorSource(train$question2[i]))
  docs2 <- tm_map(docs2, removePunctuation) 
  docs2 <- tm_map(docs2, removeNumbers) 
  docs2 <- tm_map(docs2, tolower) 
  docs2 <- tm_map(docs2, stemDocument) 
  docs2 <- tm_map(docs2, stripWhitespace) 
  docs2 <- tm_map(docs2, removeWords, stopwords("english")) 
  doc = TermDocumentMatrix(docs2) 
  b11 = doc$dimnames$Terms
  c11 = a11 %in% b11
  same_items = sum(c11)
  distinct_items = length(a11) + length(b11)
  match_count = (2*same_items)/(distinct_items)
  sentiment1 <- get_nrc_sentiment(train$question1[i])
  sentiment2 <- get_nrc_sentiment(train$question2[i])
  
  sentiment1
  sentiment2
  p1 = sum(sentiment1$positive)
  p2 = sum(sentiment2$positive)
  
  n1 = sum(sentiment1$negative)
  n2 = sum(sentiment2$negative)
  df.new = cbind(match_count,p1,p2,n1,n2)
  df = rbind(df,df.new)
  print(i)
}

tr = cbind(train,df)
head(tr)

tr = tr[,6:11]
tr = na.omit(tr)
tr$is_duplicate = as.factor(as.character(tr$is_duplicate))
logistic<-lm(is_duplicate ~ ., data = tr,family="gaussian")
predict_log<-predict(logistic,newdata = tr[,-c(1)])
actual<-test$is_duplicate

logLoss = function(predict_log, actual){
  -1*mean(log(predict_log[model.matrix(~ actual + 0) - predict_log > 0]))
}

table(test$is_duplicate)
table(predict_log)
Metrics::logLoss(as.numeric(actual),predict_log)


predict_log[predict_log > 0.5] <- 1
predict_log[predict_log < 0.5] <- 0

## Random Forests
model <- randomForest(is_duplicate ~ ., data = tr, n.tree = 1000)
model

varImpPlot(model)
importance(model)

pred <- predict(model, newdata = tr, type = "response")
pred <- as.numeric(pred)
misclass_rf <- sum(abs(as.numeric(tr$is_duplicate) - pred)/length(pred))
misclass_rf

##Random Forests Log loss
pred <- predict(model, newdata = tr, type="prob")
logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

pred = apply(pred, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

## Logloss value
logLoss(pred, tr$is_duplicate)

## XG Boost 
library(xgboost)
xgboost_tr = tr
xgboost_tr = data.frame(lapply(xgboost_tr, function(x) as.numeric(as.character(x))))
train_target = xgboost_tr$is_duplicate
train_matrix = as.matrix(xgboost_tr[,-c(1)])
xgb.model = xgboost(data = train_matrix, label = train_target, eta = 0.1, max_depth = 5, 
                    objective = "binary:logistic", eval_metric = "logloss", nrounds = 100)
xgb.model

## Test data set
df1 = data.frame()
df1.new = data.frame()

for (i in 1:nrow(test))
{
  docs1 <- Corpus(VectorSource(test$question1[i]))
  docs1 <- tm_map(docs1, removePunctuation) 
  docs1 <- tm_map(docs1, removeNumbers) 
  docs1 <- tm_map(docs1, tolower)
  docs1 <- tm_map(docs1, stemDocument) 
  docs1 <- tm_map(docs1, stripWhitespace) 
  docs1 <- tm_map(docs1, removeWords, stopwords("english")) 
  doc = TermDocumentMatrix(docs1) 
  a11 = doc$dimnames$Terms
  docs2 <- Corpus(VectorSource(test$question2[i]))
  docs2 <- tm_map(docs2, removePunctuation) 
  docs2 <- tm_map(docs2, removeNumbers) 
  docs2 <- tm_map(docs2, tolower) 
  docs2 <- tm_map(docs2, stemDocument) 
  docs2 <- tm_map(docs2, stripWhitespace) 
  docs2 <- tm_map(docs2, removeWords, stopwords("english")) 
  doc = TermDocumentMatrix(docs2) 
  b11 = doc$dimnames$Terms
  c11 = a11 %in% b11
  same_items = sum(c11)
  distinct_items = length(a11) + length(b11)
  match_count = (2*same_items)/(distinct_items)
  sentiment1 <- get_nrc_sentiment(test$question1[i])
  sentiment2 <- get_nrc_sentiment(test$question2[i])
  
  sentiment1
  sentiment2
  p1 = sum(sentiment1$positive)
  p2 = sum(sentiment2$positive)
  
  n1 = sum(sentiment1$negative)
  n2 = sum(sentiment2$negative)
  df1.new = cbind(match_count,p1,p2,n1,n2)
  df1 = rbind(df1,df1.new)
  print(i)
}

tr1 = cbind(test,df1)
head(tr1)

tr1 = tr1[,6:11]
typeof(tr1)
typeof(tr)

## Test Error of Random Forests
pred_test <- predict(model, newdata = tr1[,-c(1)],type="response")
pred_test <- as.numeric(pred_test)

misclass_rf <- sum(abs(as.numeric(tr1$is_duplicate) - pred_test)/length(pred_test))
misclass_rf

## Test Error of XG Boosting
xgboost_tr1 = tr1
xgboost_tr1 = data.frame(lapply(xgboost_tr1, function(x) as.numeric(as.character(x))))
test_target = xgboost_tr1$is_duplicate
test_matrix = as.matrix(xgboost_tr1[,-c(1)])

pred_test_xg<-predict(xgb.model,newdata=test_matrix)
pred_test_xg<-as.numeric(pred_test_xg)
pred_test_xg[pred_test_xg > 0.5] <- 1
pred_test_xg[pred_test_xg < 0.5] <- 0

table(pred_test_xg)

misclass_xg <- sum(abs(as.numeric(test_target) - pred_test_xg)/length(pred_test_xg))
misclass_xg