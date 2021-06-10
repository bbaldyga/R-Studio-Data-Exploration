Sys.setenv(LANG="en")
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
#prepare data
iris <-read.csv("/Users/blazejbaldyga/Downloads/DrzewaDecyzyjne/iris.csv")
str(iris)
iris <- mutate_at(iris,vars(Classification),list(as.factor))
str(iris)

#split into train and test sets
set.seed(1456)
trainIndex <- createDataPartition(iris$Classification,p = 0.75, list = FALSE, times = 1)
sTrain <- iris[trainIndex,]
sTest <- iris[-trainIndex,]
count(sTrain,Classification)

#make ML model
#-minsplit
#-maxdepth
#-xval
#-cp

#create tree with xval-fold cross validation
dt_control <- rpart.control(maxdepth=25, xval=10, cp=0)
d_tree <- rpart(Classification~., data = sTrain, method = "class", control = dt_control, minsplit = 5)#tree

#select optimal cp parameter value
printcp(d_tree)
plotcp(d_tree)

#prune tree with choosen parameter cp, plot pruned tree
dl_tree <- prune(d_tree, cp = 0)
rpart.plot(dl_tree, extra = 101, fallen.leaves = FALSE, tweak = 1.0, varlen = 10, faclen = 10,box.palette = "lightblue")

#test the pruned tree preformance
dl_tree_predict <- predict(dl_tree, sTest, type="class")
confusionMatrix(table(dl_tree_predict,sTest$Classification))