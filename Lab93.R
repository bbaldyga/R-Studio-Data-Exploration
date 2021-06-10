Clean2<-scale(Clean1[3:168])
View(Clean2)
k1<-kmeans(Clean2,centers = 1, nstart = 25)
k2<-kmeans(Clean2,centers = 2, nstart = 25)
k3<-kmeans(Clean2,centers = 3, nstart = 25)
k4<-kmeans(Clean2,centers = 4, nstart = 25)
k5<-kmeans(Clean2,centers = 5, nstart = 25)
k6<-kmeans(Clean2,centers = 6, nstart = 25)
k7<-kmeans(Clean2,centers = 7, nstart = 25)
k8<-kmeans(Clean2,centers = 8, nstart = 25)
k9<-kmeans(Clean2,centers = 9, nstart = 25)
k10<-kmeans(Clean2,centers = 10, nstart = 25)
k11<-kmeans(Clean2,centers = 11, nstart = 25)
k12<-kmeans(Clean2,centers = 12, nstart = 25)
k13<-kmeans(Clean2,centers = 13, nstart = 25)
k14<-kmeans(Clean2,centers = 14, nstart = 25)
k15<-kmeans(Clean2,centers = 15, nstart = 25)

str(k2)
k2

install.packages("factoextra") ## do obliczenia i wizualizacji 
library(factoextra)

p1 <- fviz_cluster(k2, geom = "point", data = Clean2) + ggtitle("k = 2") 
p2 <- fviz_cluster(k3, geom = "point", data = Clean2) + ggtitle("k = 3")
library(gridExtra)
grid.arrange(p1, p2, nrow = 1) ## dwa wykresy na jednym rysunku

p3 <- fviz_cluster(k4, geom = "point", data = Clean2) + ggtitle("k = 4") 
p4 <- fviz_cluster(k5, geom = "point", data = Clean2) + ggtitle("k = 5")
grid.arrange(p3, p4, nrow = 1) ## dwa wykresy na jednym rysunku

p5 <- fviz_cluster(k6, geom = "point", data = Clean2) + ggtitle("k = 6") 
p6 <- fviz_cluster(k7, geom = "point", data = Clean2) + ggtitle("k = 7")
grid.arrange(p5, p6, nrow = 1) ## dwa wykresy na jednym rysunku

fviz_nbclust(Clean2, kmeans, method = "wss") ## miara wss - within-cluster sum of square

fviz_nbclust(Clean2, kmeans, method = "silhouette")

library(cluster)
gap_stat <- clusGap(Clean2, FUN = kmeans, nstart = 25, K.max = 20, B = 50) ## Oblicz statystykę luk
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)