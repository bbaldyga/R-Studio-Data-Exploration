for(j in 3:(ncol(clean1)-1))
{ 
  suma<-0
  qt<-quantile(clean1[,j], probs=c(.25,.75))
  suma <- sum(clean1[,j] < (qt[1]-1.5 * IQR(clean1[,j])))
  suma <- suma+ sum(clean1[,j] > (qt[2]+1.5 * IQR(clean1[,j])))
  print(colnames(clean1[j]))
  print(suma)
  hist(clean1[,j], main = "Histogram", xlab = colnames(clean1[j]) )
}

