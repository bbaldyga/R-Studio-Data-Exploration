Clean1BezTekst<-Clean1_Nowy[3:169]
no_all<-nrow(Clean1BezTekst) ## liczba wszystkich obserwacji w zbiorze 
no_sampled<-floor(no_all*0.65) ## liczba obserwacji losowanych do zbioru uczącego
set.seed(123) ## Ustawiamy ziarno 
trainsub<-c(sample(1:no_all, no_sampled))## losujemy INDEKSY obserwacji
testsub<-setdiff(c(1:no_all),trainsub)
Clean1_train<-Clean1BezTekst[trainsub,]
Clean1_test<-Clean1BezTekst[testsub,]
Clean1_train$Klassa <- as.factor(Clean1_train$Klassa)
## do zbioru uczącego ##pobieramypozostałeINDEKSY
## obserwacji do zbioru testowego ## generujemy zbiór uczący
## generujemy zbiór testowy

##Bagging
#library(ipred)
#library(rpart)
bag<-bagging(Klassa~., Clean1_train, nbagg=100, minsplit=5, cp=0.001)
## ta instrukcja ma długi czas wykonania!
bag<-prune.rpart(bag, cp=0.01)
bagwynik<-predict(bag, Clean1_test)

kbag<-table(bagwynik$class,Clean1_test$Klassa) ##macierzbłędów
print(kbag)
accbag<-100*sum(diag(kbag)/sum(kbag)) ## % poprawnych zaklasyfikowań
print(accbag)
errbag<-bl<-100-100*sum(diag(kbag)/sum(kbag)) ##%błędnychzaklasyfikowań
print(errbag)

#install.packages("adabag")
#library(adabag)
ustawienia=rpart.control(minsplit=5, cp=0.001)
TestDoCleana<-Clean1_train

boost <- boosting(Klassa ~ ., data = Clean1_train, boos = TRUE, mfinal = 9, control = (minsplit = 0))
boost <- boosting(Klassa~.,Clean1_train, mfinal=100, control=ustawienia)
## ta instrukcja ma długi czas wykonania!
boost <- prune.rpart(boost, cp=0.01)
boostwynik <- predict(boost, Clean1_test)
#Analiza wyników:
kboost<-table(boostwynik$class,Clean1_test$Klassa) ## macierz błędów
print(kboost)
accboost<-100*sum(diag(kboost)/sum(kboost)) ## % poprawnych zaklasyfikowań
print(accboost)
errboost<-bl<-100-100*sum(diag(kboost)/sum(kboost))## % błędnych zaklasyfikowńa
print(errboost)

#install.packages("randomForest")
#library(randomForest)
rndf <-randomForest(Klassa~., Clean1_train, ntree=100, cp=0.001, keep.forest=TRUE, importance=TRUE,na.action = na.exclude)
rndf <- prune.rpart(rndf, cp=0.01)
rndfwynik <- predict(rndf, Clean1_test)
#Analiza wyników:
krndf<-table(rndfwynik,Clean1_test$Klassa)
print(krndf)
accrndf<-100*sum(diag(krndf)/sum(krndf))
print(accrndf)
errrndf<-bl<-100-100*sum(diag(krndf)/sum(krndf))## % błędnych zaklasyfikow
print(errrndf)

no_all<-nrow(Clean1Lista6) ## liczba wszystkich obserwacji w zbiorze 
no_sampled<-floor(no_all*0.65) ## liczba obserwacji losowanych do zbioru uczącego
set.seed(123) ## Ustawiamy ziarno 
trainsub<-c(sample(1:no_all, no_sampled))## losujemy INDEKSY obserwacji
testsub<-setdiff(c(1:no_all),trainsub)
Clean1_train<-Clean1BezTekst[trainsub,]
Clean1_test<-Clean1BezTekst[testsub,]

## do zbioru uczącego ##pobieramypozostałeINDEKSY
## obserwacji do zbioru testowego ## generujemy zbiór uczący
## generujemy zbiór testowy

##Bagging
#library(ipred)
#library(rpart)
Clean1_train$Klassa <- as.factor(Clean1_train$Klassa)
bag<-bagging(Klassa~., Clean1_train, nbagg=100, minsplit=5, cp=0.001)
## ta instrukcja ma długi czas wykonania!
bag<-prune.rpart(bag, cp=0.01)
bagwynik2<-predict(bag, Clean1_test)

kbag2<-table(bagwynik2$class,Clean1_test$Klassa) ##macierzbłędów
print(kbag2)
accbag2<-100*sum(diag(kbag2)/sum(kbag2)) ## % poprawnych zaklasyfikowań
print(accbag2)
errbag2<-bl<-100-100*sum(diag(kbag2)/sum(kbag2)) ##%błędnychzaklasyfikowań
print(errbag2)

#install.packages("adabag")
#library(adabag)
ustawienia=rpart.control(minsplit=5, cp=0.001)
TestDoCleana<-Clean1_train
Clean1_train$Klassa <- as.factor(Clean1_train$Klassa)
boost <- boosting(Klassa ~ ., data = Clean1_train, boos = TRUE, mfinal = 9, control = (minsplit = 0))
boost <- boosting(Klassa~.,Clean1_train, mfinal=100, control=ustawienia)
## ta instrukcja ma długi czas wykonania!
boost <- prune.rpart(boost, cp=0.01)
boostwynik <- predict(boost, Clean1_test)
#Analiza wyników:
kboost2<-table(boostwynik$class,Clean1_test$Klassa) ## macierz błędów
print(kboost2)
accboost2<-100*sum(diag(kboost2)/sum(kboost2)) ## % poprawnych zaklasyfikowań
print(accboost2)
errboost2<-bl<-100-100*sum(diag(kboost2)/sum(kboost2))## % błędnych zaklasyfikowńa
print(errboost2)

##install.packages("randomForest")
##library(randomForest)
rndf <-randomForest(Klassa~., Clean1_train, ntree=100, cp=0.001, keep.forest=TRUE, importance=TRUE,na.action = na.exclude)
rndf <- prune.rpart(rndf, cp=0.01)
rndfwynik <- predict(rndf, Clean1_test)
#Analiza wyników:
krndf2<-table(rndfwynik,Clean1_test$Klassa)
print(krndf2)
accrndf2<-100*sum(diag(krndf2)/sum(krndf2))
print(accrndf2)
errrndf2<-bl<-100-100*sum(diag(krndf2)/sum(krndf2))## % błędnych zaklasyfikow
print(errrndf2)
barplot()
errvector<-c(accbag,accbag2,accboost,accboost2,accrndf,accrndf2)
barplot(errvector,main="dokładność klasyfikacji nadzorowanej", xlab="Metoda",names.arg=c("Bagging","Bagging2","Boosting","Boosting2", "Losowy las", "Losowy las2"))
