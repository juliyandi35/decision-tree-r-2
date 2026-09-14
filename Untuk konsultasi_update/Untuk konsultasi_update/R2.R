# What are the influencing factors for batik consumers to buy traditional batik products in the future?
library(tree) #Load tree library to construct classification trees
library(readxl)
Data.R2 <- read_excel("R2.xlsx")
head(Data.R2)

Data.R2 <- na.omit(Data.R2)
head(Data.R2)
str(Data.R2)
Data.R2 <- data.frame(Data.R2[,-1])

for (i in 1:ncol(Data.R2)){
  Data.R2[,i] <- factor(Data.R2[,i])
}

str(Data.R2)

# Split data
set.seed(123)
nrow(Data.R2)
n = runif(1, 50, 70)
train=sample(1:nrow(Data.R2), n)  #Row sample for training data
head(train)

#Dataset for testing
Data.R2.test = Data.R2[-train ,]

tree.R2 = tree(Buy_Freq~., Data.R2, subset = train)

#Summarize the classification tree model
summary(tree.R2)

#Text visualization of model
tree.R2

#Tree visualization of model
plot(tree.R2)
text(tree.R2,cex=1/2)

#Predict the model using data testing Carseats.test
set.seed(123)
tree.pred=predict(tree.R2,Data.R2.test, type ="class")
head(tree.pred, 5)

#Generate the confusion matrix showing proportions from testing data .
print("confusion matrix showing test proportion")
per <- rattle::errorMatrix(Data.R2.test$Buy_Freq, tree.pred, count = FALSE)
per

#Calculate the testing overall error percentage.
print("overall error percentage")
cat(100-sum(diag(per), na.rm=TRUE))

#Calculate deviance
set.seed (123)
cv.R2 = cv.tree(tree.R2, FUN=prune.misclass, K = 10)
cv.R2

#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.R2$size ,cv.R2$dev ,type="b")
plot(cv.R2$k ,cv.R2$dev ,type="b")

#Prune the best size
set.seed(123)
best_size = 3
prune.R2 = prune.misclass(tree.R2 , best = best_size)

#Summary of pruning result
summary(prune.R2)

#Text visualization
prune.R2

#Tree visualization
par(mfrow=c(1,1))
plot(prune.R2 )
text(prune.R2 ,col=1, cex=1/2)

#Predict using pruned tree model
set.seed(123)
tree_pred = predict(prune.R2, Data.R2.test, type="class")

#Generate the confusion matrix showing counts for pruned tree.
print("confusion matrix showing test counts")
per_pruned <- rattle::errorMatrix(Data.R2.test$Buy_Freq, tree_pred, count=FALSE)
per_pruned

#Overall error
print("overall error percentage")
cat(100-sum(diag(per_pruned), na.rm=TRUE))
