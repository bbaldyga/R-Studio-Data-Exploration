 res.cor <- cor(Clean1)
 View(res.cor)
 corrplot(res.cor)

 CreateRanking<-function(w)
 {
         weightframe <- data.frame(features = row.names(w), w, row.names=NULL)
         weightframewithranks <- transform(weightframe, importance = rank(- attr_importance, ties.method = "first"))
         ranks <- weightframewithranks[,-2]
         ranks }
rm(weights1)
x <- subset(Clean1, select = -Klassa)
weights1 <- cor(Clean1, Clean1$Klassa, method = "pearson")
print(weights1)
weights <- abs(weights1)
print(weights1)
colnames(weights1)<-c("attr_importance")
R1<-CreateRanking(weights1)
weights2 <- chi.squared(Klassa~., Clean1) 
print(weights2)
R2<-CreateRanking(weights2)

weights3 <- information.gain(Klassa~., Clean1) 
print(weights3)
R3<-CreateRanking(weights3)

weights4 <- gain.ratio(Klassa~., Clean1) 
print(weights4)
R4<-CreateRanking(weights4)

weights5 <- symmetrical.uncertainty(Klassa~., Clean1) 
print(weights5)
R5<-CreateRanking(weights5)

weights6 <- oneR(Klassa~., Clean1)
print(weights6)
R6<-CreateRanking(weights6)

set.seed(7)
Clean1$Klassa=as.factor(Clean1$Klassa)
weights7 <- relief(Klassa~., Clean1, neighbours.count = 10, sample.size = 10)
print(weights7)
R7<-CreateRanking(weights7)
weights = weights[-167,]
View(weights)
colnames(weights)<-c("attr_importance")
CreateRanking(weights)
weights2 = weights2[-(1:2),]
View(weights2)
colname(weights2)<-c("attr_importance")
CreateRanking(weights2)
weights3 = weights3[-(1:2),]
View(weights3)
weights4 = weights4[-(1:2),]
View(weights4)
weights5 = weights5[-(1:2),]
View(weights5)
weights6 = weights6[-(1:2),]
View(weights6)
weights7 = weights7[-(1:2),]
View(weights7)

test<-cbind(R2$importance,R3$importance,R4$importance,R5$importance,R6$importance,R7$importance)
rownames(test) <-R2$features
colnames(test) <-c("Chi-squared filter","Information gain","Gain ratio","Symmetrical uncertainty","OneR","Relief")
View(test)
TablicaZeSred<-cbind(test,rowMeans(test))
colnames(TablicaZeSred) <-c("Chi-squared filter","Information gain","Gain ratio","Symmetrical uncertainty","OneR","Relief","Srednia")
TablicaZeSred[order(TablicaZeSred[,7],decreasing = FALSE),]
View(TablicaZeSred)
