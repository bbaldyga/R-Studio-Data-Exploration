Sys.setenv(LANG="en")
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
#prepare data
wine <-read.csv("/Users/blazejbaldyga/Downloads/DrzewaDecyzyjne/wine.csv")
str(wine)
wine <- suppressWarnings(mutate_at(wine,vars(Alcohol),list(as.numeric)))
wine <- mutate_at(wine,vars(Class),list(as.factor))
wine <-wine[-155,]
str(wine)

#split into train and test sets
set.seed(1456)
trainIndex <- createDataPartition(wine$Class,p = 0.75, list = FALSE, times = 1)
sTrain <- wine[trainIndex,]
sTest <- wine[-trainIndex,]
count(sTrain,Class)

#make ML model
#-minsplit
#-maxdepth
#-xval
#-cp

#create tree with xval-fold cross validation
dt_control <- rpart.control(maxdepth=25, xval=10, cp=0)
d_tree <- rpart(Class~., data = sTrain, method = "class", control = dt_control, minsplit = 5)#tree

#select optimal cp parameter value
printcp(d_tree)
plotcp(d_tree)

#prune tree with choosen parameter cp, plot pruned tree
dl_tree <- prune(d_tree, cp = 0.000000)
rpart.plot(dl_tree, extra = 101, fallen.leaves = FALSE, tweak = 1.0, varlen = 10, faclen = 10)

#test the pruned tree preformance
dl_tree_predict <- predict(dl_tree, sTest, type="class")
confusionMatrix(table(dl_tree_predict,sTest$Class))
