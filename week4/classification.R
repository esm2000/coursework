library(tidyverse)
library(scales)
library(ElemStatLearn) # spam dataset
library(e1071)         # Naive Bayes implementation
library(ROCR)          # evaluation metrics

theme_set(theme_bw())

str(spam)
summary(spam)
set.seed(42) # reproducible results
# Clever way to split the data into training and testing data
# Validation being ignored for right now
ndx <- sample(nrow(spam), floor(nrow(spam) * 0.9))
train <- spam[ndx,]
test <- spam[-ndx,]

xTrain <- train[,-58]
yTrain <- train$spam
xTest <- test[,-58]
yTest <- test$spam

# -------------------------------------------------------------
# Naive Bayes no smoothing
# -------------------------------------------------------------
model <- naiveBayes(xTrain, yTrain)
summary(model)
# Length Class  Mode     
# apriori    2     table  numeric  
# tables    57     -none- list     
# levels     2     -none- character
# isnumeric 57     -none- logical  
# call       3     -none- call

df <- data.frame(actual = yTest, pred = predict(model,xTest))
head(df)
# actual pred
# 1   spam spam
# 2   spam spam
# 3   spam spam
# 4   spam spam
# 5   spam spam
# 6   spam spam

table(df)

# accuracy: fraction of correct classifications
df %>%
  summarize(acc = mean(pred == actual))

# precision; fraction of positive predictions that are actually true
df %>%
  filter(pred == 'spam') %>%
  summarize(prec = mean(actual == 'spam'))

# recall: fraction of true examples that we predicted to 
# be positive
# aka true positive rate, sensitivity
df %>%
  filter(actual == 'spam') %>%
  summarize(recall = mean(pred == 'spam'))

# false positive rate: fraction of false examples that we 
# predicted to be positive
df %>%
  filter(actual == 'email') %>%
  summarize(fpr = mean(pred == 'spam'))

# plot histogram of predictied possibilities
# note overconfident predicitons
probs <- data.frame(predict(model, xTest, type = 'raw'))

ggplot(probs, aes(x = spam)) +
  geom_histogram(binwidth = 0.01) + 
  scale_x_continuous(label = percent) + 
  xlab("Predicted probability of spam") + 
  ylab('Number of examples')

# check calibration by looking at how often predicted probabilties
# match actual frequencies

# This is most easily done by binning examples by their predicted
# probability of being spam and then counting how often those
# examples actually turn out to be spam

data.frame(predicted=probs[, "spam"], actual=yTest) %>%
  group_by(predicted=round(predicted*10)/10) %>%
  summarize(num=n(), actual=mean(actual == "spam")) %>%
  ggplot(data=., aes(x=predicted, y=actual, size=num)) +
  geom_point() +
  geom_abline(linetype=2) +
  scale_x_continuous(labels=percent, lim=c(0,1)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  xlab('Predicted probability of spam') +
  ylab('Percent that are actually spam')

# we can use the ROCR package to make a plot of the receiver
# operator characteristic (ROC) curve and compute the area
# under the curve (AUC)

# The ROC curve plots the true positive rate (also known as
# recall, sensitivity, or the probability of a false alarm)
# as we chane the threshold on the probability for predicting spam

# create a ROCR object
pred <- prediction(probs[, "spam"], yTest)

# plot ROC curve
perf_nb <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_nb)
performance(pred, 'auc')