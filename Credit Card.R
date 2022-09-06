library(tidyverse)
library(dplyr)
library(readr)
library(caret)
library(caTools)
library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)

creditcard <- read_csv("Desktop/TAMU/Courses/Kaggle/creditcard.csv")
View(creditcard)
data <- creditcard

dim(data)
nrow(data)
ncol(data)
summary(data)
data = data.frame(data)

which(is.na(data))
sum(is.na(data))

data <- subset(data, select = -Time)
data$Amount <- scale(data$Amount)
data
table(data$Class)

Class <- as.factor(data$Class)
data <- subset(data,select = -Class)
data <- cbind(data, Class)
data

set.seed(1)

sample <- sample.split(data, SplitRatio = 0.7)
train  <- subset(data, sample == TRUE)
test   <- subset(data, sample == FALSE)

#Fitting logistic regression

?lm
lr = glm(Class~., data=train, family=binomial)
summary(lr)
lr.probs=predict(lr,newdata= test,type="response") 
lr.probs[1:10]
lr.pred=ifelse(lr.probs > 0.5, 1, 0)

#table(lr.pred, test$Class)
#mean(lr.pred==test$Class) #99.91 accuracy because of imbalanced dataset.

#Error measure
library(pROC)
?roc
lr.curve = roc(test$Class, lr.pred)
plot(lr.curve)
print(lr.curve) # AUC = 0.8213


#Fitting LDA
ld=lda(Class~.,data=train)
ld
ld.pred = predict(ld, test)
ld.pred
class(ld.pred)

#lda.pred = data.frame(ld.pred)
#table(lda.pred$class, test$Class)

typeof(test$Class)
length(test$Class)
test$Class = as.numeric(test$Class)
typeof(lda.pred)
#lda.pred=unlist(lda.pred)
length(lda.pred)

#Error measure
lda.curve = roc(test$Class, lda.pred)
plot(lda.curve)
print(lda.curve)

#Fitting tree

tr = tree(Class~., data=train)
summary(tr)
plot(tr)
text(tr, pretty=0)
set.seed(1) 
cv.train = cv.tree(tr, FUN=prune.misclass)
plot(cv.train)
prune.tr = prune.misclass(tr, best=6)
plot(prune.tr)
text(prune.tr, pretty=0)
tr.pred = predict(prune.tr, test, type="class")
#table(tr.pred, test$Class)


length(test$Class)
length(tr.pred)
length(train$Class)

test$Class <- as.numeric(test$Class)
typeof(test$Class)
typeof(tr.pred)
tr.pred<- as.numeric(tr.pred)
typeof(tr.pred)

#Error measure
r.curve = roc(test$Class, tr.pred)
plot(r.curve)
print(r.curve) # AUC = 0.883

?randomForest
rf = randomForest(Class~., data=train, ntree=500, mtry=5)
plot(rf)
rf # OOB = 0.05%

#trf.pred=predict(rf, test, type="class")
#trf.pred=as.numeric(trf.pred)
#typeof(trf.pred)

#Error meaure
#rf.curve = roc(test$Class, trf.pred)
#plot(r.curve)
#print(r.curve)

#Fitting KNN

library(class)
?knn
knn.pred=knn(train,test,train$Class, k=3)
knn.pred
typeof(knn.pred)
knn.pred=as.numeric(knn.pred)


#Error measure
knn.curve = roc(test$Class, knn.pred)
plot(knn.curve)
print(knn.curve) # AUC = 0.909

x<-(1:10)
x
for (k in x){
  knn.pred=knn(train,test,train$Class, k=k)
  knn.pred=as.numeric(knn.pred)
  knn.curve = roc(test$Class, knn.pred)
  print(knn.curve)
}
