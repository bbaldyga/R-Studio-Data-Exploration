install.packages('TSclust') 
library("TSclust")
library("cluster")
data("interest.rates")
View(interest.rates)
diss.PRED(interest.rates[,10], interest.rates[,14], h = 6, B = 2000,
logarithm.x = TRUE, logarithm.y = TRUE, differences.x = 1, differences.y = 1, plot = TRUE)$L1dist

relative.rate.change <- diff(log(interest.rates), 1) ## (zostaje usunięta niestacjonarność oryginalnych szeregów poprzez różniczkowanie logarytmów)
Five.cluster.sol <- matrix(0, nrow = 18, ncol = 3) ## utworzenie pustej macierzy na dane 5 klastrów
colnames(Five.cluster.sol) <- c("ACF", "LNP", "PIC") ## każda kolumna będzie dla innej miary odległości
rownames(Five.cluster.sol) <- colnames(relative.rate.change) ## każdy wiersz to dane innego kraju
##Przeprowadzamy grupowanie do 5 klastrów przy użyciu trzech miar odległości, wyniki zapisujemy do macierzy:
Five.cluster.sol[, 1] <- cutree(hclust(diss(relative.rate.change, "ACF", p = 0.05)), k = 5)
Five.cluster.sol[, 2] <- cutree(hclust(diss(relative.rate.change, "PER", normalize = TRUE, logarithm =
TRUE)), k = 5)
Five.cluster.sol[, 3] <- cutree(hclust(diss(relative.rate.change, "AR.PIC")), k = 5)
Five.cluster.sol

ClustPurity<- function(cluster) {
    sum<-0
    k<-5 
    M<-length(cluster)
      for (i in 1:k)
      {
        m<-0
        Eur<-0
        NieEur<-0
        for(j in 1:M) {
          if(cluster[j]==i) {
            m<-m+1 
            if(j==1||j==16||j==17||j==18)
              {
              NieEur<-NieEur+1 
              }
            else 
              {
                Eur<-Eur+1 
              }
          } 
      }
      sum<-sum+((m/M)*(max(Eur/m,NieEur/m))) 
    }
  return (sum) 
}

ACF<-ClustPurity(Five.cluster.sol[,1])
LNP<-ClustPurity(Five.cluster.sol[,2])
PIC<-ClustPurity(Five.cluster.sol[,3])
ACF
LNP
PIC
plot(c(ACF,LNP,PIC),type = "l",col = "blue", xlab = "Przypadki", ylab = "Wartosc", main = "Czystość Klastrowania")
