def.par<-par(no.readonly = T)
x<-Clean1[,3]
x1<-discretize(x,method = "interval",breaks = 3)
x1
plot(x1,xlab = "Cecha odleglosci1", ylab ="Częstość")
x2<-discretize(x, method = "interval",breaks = 3, labels = c("mala","srednia","duza"))
plot(x2,xlab = "Cecha odleglosci1", ylab = "Czestosc")
hist(x, breaks = 20, main = "Rowne szerokosci przedzialow",xlab = "cecha odlegloscci 1", ylab = "Czestosc")
abline(v = discretize(x, method = "interval", breaks = 3,onlycuts = TRUE),col = "red")

x3<-discretize(x, method = "frequency",breaks = 3,labels = c("mala", "srednia" ,"duza"))
plot(x3,xlab="cecha odleglosci 1", ylab="Czestosc")
hist(x,breaks = 20,main = "Rowne czestosci", xlab = "dlugosc platka", ylab = "Czestosc")
abline(v = discretize(x, methid = "frequency",breaks = 3, onlycuts = T),col = "red")

Clean1Num<-Clean1[,3:168]
View(Clean1Num)
Clean1Disc<-discretizeDF(Clean1Num)
Clean1Disc <-discretizeDF(Clean1Num, default = list(method = "interval", breaks =2, labels=c("small", "large")))
head(Clean1Disc)

Clean2<-discretizeDF(Clean1Num)
head(Clean2)
rules <- apriori(Clean2,parameter = list(support = .4,confidence = .8))
rules
frequentSets<- apriori(Clean2, parameter = list(target = "frequent",support = .4,confidence = .8, minlen = 2, maxlen = 4))
frequentSets
inspect(sort(frequentSets,by = "support"))
inspect(head(sort(rules, by = "lift"), 10))

plot(rules)
