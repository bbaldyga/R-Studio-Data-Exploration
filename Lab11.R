no_all<-nrow(Clean1)
no_sampled<-floor(no_all*0.7)
set.seed(123)
trainsub<-c(sample(1:no_all, no_sampled))
testsub<-setdiff(c(1:no_all),trainsub)
trainsub
testsub
Clean_train<-Clean1[trainsub,]
Clean_test<-Clean1[testsub,]