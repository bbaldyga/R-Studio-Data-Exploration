library("e1071")
library("arules")
library("	magrittr")
smp_size <-floor(0.8*nrow(Clean1))
smp_size
set.seed(123)
train_ind <-sample(seq_len(nrow(Clean1)),size = smp_size)
train_ind

Clean1_train<-Clean1[train_ind,]
Clean1_test<-Clean1[-train_ind,]
View(Clean1_test)
View(Clean1_train)
#musiałem zastosować drugi sposób ponieważ na początku mam zmienne nie numeryczne
m <- naiveBayes(Clean1_train[,-167],Clean1_train[,167])
m

#, gdzie: newD_test to tabela z danymi
wyniki<-table(predict(m,Clean1_test),Clean1_test[,167])
wyniki

m <- naiveBayes(Clean1_test[,-167],Clean1_test[,167])
m
Clean1_test$Klassa<-factor(Clean1_test$Klassa)
wyniki<-table(predict(m,Clean1_test),Clean1_test[,167])
wyniki
Clean1BezNazw<-Clean1[,3:169]
smp_size <-floor(0.8*nrow(Clean1BezNazw))
smp_size
set.seed(123)
train_ind <-sample(seq_len(nrow(Clean1BezNazw)),size = smp_size)
train_ind

Clean1_train<-Clean1BezNazw[train_ind,]
Clean1_test<-Clean1BezNazw[-train_ind,]
View(Clean1_test)
View(Clean1_train)
Clean1_test[-167] = scale(Clean1_train[-167]) ## pomijamy zmienną celu (kolumna 5-ta)
Clean1_test[-167] = scale(Clean1_train[-167])
library("e1071")
modelR<-svm(Klassa~.,data=Clean1_train,type = "C-classification",kernel="radial")
modelL<-svm(Klassa~.,data=Clean1_train,type = "C-classification",kernel="linear")
modelP<-svm(Klassa~.,data=Clean1_train,type = "C-classification",kernel="polynomial")
modelS<-svm(Klassa~.,data=Clean1_train,type = "C-classification",kernel="sigmoid")
modelR
modelL
modelP
modelS

test.pred<-predict(modelR,Clean1_test)
kl<-table(test.pred,Clean1_test$Klassa)
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelL,Clean1_test)
kl<-table(test.pred,Clean1_test$Klassa)
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelP,Clean1_test)
kl<-table(test.pred,Clean1_test$Klassa)
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelS,Clean1_test)
kl<-table(test.pred,Clean1_test$Klassa)
print(kl)
print(100*sum(diag(kl)/sum(kl)))





#do IRYSA
data(iris)
View(iris)
smp_size <- floor(0.7 * nrow(iris))## 70% danych
smp_size
set.seed(123) ## Ustawiamy ziarno w celu zapewnienia
## powtarzalności podziału zbioru
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train_ind
iris_train <- iris[train_ind, ] 
iris_test <- iris[-train_ind, ]
m <- naiveBayes(Species~., data = iris_train)
wynik<-table(predict(m, iris_test), iris_test[,5])
wynik


iris_train[-5] = scale(iris_train[-5]) ## pomijamy zmienną celu (kolumna 5-ta)
iris_test[-5] = scale(iris_test[-5])
library("e1071")
modelR<-svm(Species~.,data=iris_train,type = "C-classification",kernel="radial")
modelL<-svm(Species~.,data=iris_train,type = "C-classification",kernel="linear")
modelP<-svm(Species~.,data=iris_train,type = "C-classification",kernel="polynomial")
modelS<-svm(Species~.,data=iris_train,type = "C-classification",kernel="sigmoid")

test.pred<-predict(modelR,iris_test)
kl<-table(test.pred,iris_test$Species) 
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelL,iris_test)
kl<-table(test.pred,iris_test$Species) 
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelP,iris_test)
kl<-table(test.pred,iris_test$Species) 
print(kl)
print(100*sum(diag(kl)/sum(kl)))

test.pred<-predict(modelS,iris_test)
kl<-table(test.pred,iris_test$Species) 
print(kl)
print(100*sum(diag(kl)/sum(kl)))

