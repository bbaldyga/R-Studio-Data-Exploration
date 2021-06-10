for(i in 3:(ncol(clean1)-1))
{
  qt<-quantile(clean1[,i], probs=c(.25,.75))
  clean1[,i][clean1[,i]<(qt[1]-1.5*IQR(clean1[,i]))]<-NA
  clean1 <- clean1[complete.cases(clean1),]
  clean1[,i][clean1[,i]>(qt[2]+1.5*IQR(clean1[,i]))]<-NA
  clean1 <- clean1[complete.cases(clean1),]
}
