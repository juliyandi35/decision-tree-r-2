# What are the influencing factors for batik consumers to buy traditional batik products in the future?
library(tree) #Load tree library to construct classification trees
library(readxl)
Data.R1 <- read_excel("R1.xlsx")
head(Data.R1)

Data.R1 <- na.omit(Data.R1)
head(Data.R1)
str(Data.R1)
Data.R1 <- data.frame(Data.R1)

for (i in 1:ncol(Data.R1)){
  Data.R1[,i] <- factor(Data.R1[,i])
}

str(Data.R1)

# Split data
set.seed(123)
nrow(Data.R1)
n = runif(1, 200, 300)
train=sample(1:nrow(Data.R1), n)  #Row sample for training data
head(train)

#Dataset for testing
Data.R1.test = Data.R1[-train ,]

tree.R1.Buy.Freq = tree(Buy_Freq~., Data.R1[,-1], subset = train)

#Summarize the classification tree model
summary(tree.R1.Buy.Freq)

#Text visualization of model
tree.R1.Buy.Freq

#Tree visualization of model
plot(tree.R1.Buy.Freq)
text(tree.R1.Buy.Freq,cex=1/2)

#Predict the model using data testing Carseats.test
set.seed(123)
tree.pred.Buy.Freq=predict(tree.R1.Buy.Freq,Data.R1.test[,-1], type ="class")
head(tree.pred.Buy.Freq)

#Generate the confusion matrix showing proportions from testing data .
print("confusion matrix showing test proportion")
per.Buy.Freq <- rattle::errorMatrix(Data.R1.test$Buy_Freq, tree.pred.Buy.Freq, count = FALSE)
per.Buy.Freq

#Calculate the testing overall error percentage.
print("overall error percentage")
cat(100-sum(diag(per.Buy.Freq), na.rm=TRUE))

#Calculate deviance
set.seed(123)
cv.R1.Buy.Freq = cv.tree(tree.R1.Buy.Freq, FUN=prune.misclass, K = 10)
cv.R1.Buy.Freq

#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.R1.Buy.Freq$size ,cv.R1.Buy.Freq$dev ,type="b")
plot(cv.R1.Buy.Freq$k ,cv.R1.Buy.Freq$dev ,type="b")

#Prune the best size
set.seed(123)
best_size.Buy.Freq = 3
prune.R1.Buy.Freq = prune.misclass(tree.R1.Buy.Freq , best = best_size.Buy.Freq)

#Summary of pruning result
summary(prune.R1.Buy.Freq)

#Text visualization
prune.R1.Buy.Freq

#Tree visualization
par(mfrow=c(1,1))
plot(prune.R1.Buy.Freq)
text(prune.R1.Buy.Freq ,col=1, cex=1/2)

#Predict using pruned tree model
set.seed(123)
tree_pred.Buy.Freq = predict(prune.R1.Buy.Freq, Data.R1.test[,-1], type="class")

#Generate the confusion matrix showing counts for pruned tree.
print("confusion matrix showing test counts")
per_pruned.Buy.Freq <- rattle::errorMatrix(Data.R1.test$Buy_Freq, tree_pred.Buy.Freq, count=FALSE)
per_pruned.Buy.Freq

#Overall error
print("overall error percentage")
cat(100-sum(diag(per_pruned.Buy.Freq), na.rm=TRUE))

# Decision
tree.R1.Decision = tree(Decision~., Data.R1[,-2], subset = train)

#Summarize the classification tree model
summary(tree.R1.Decision)

#Text visualization of model
tree.R1.Decision

#Tree visualization of model
plot(tree.R1.Decision)
text(tree.R1.Decision,cex=1/2)

#Predict the model using data testing Carseats.test
set.seed(123)
tree.pred.Decision=predict(tree.R1.Decision,Data.R1.test[,-2], type ="class")
head(tree.pred.Decision)

#Generate the confusion matrix showing proportions from testing data .
print("confusion matrix showing test proportion")
per.Decision <- rattle::errorMatrix(Data.R1.test$Decision, tree.pred.Decision, count = FALSE)
per.Decision

#Calculate the testing overall error percentage.
print("overall error percentage")
cat(100-sum(diag(per.Decision), na.rm=TRUE))

#Calculate deviance
set.seed(123)
cv.R1.Decision = cv.tree(tree.R1.Decision, FUN=prune.misclass, K = 10)
cv.R1.Decision

#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.R1.Decision$size ,cv.R1.Decision$dev ,type="b")
plot(cv.R1.Decision$k ,cv.R1.Decision$dev ,type="b")

#Prune the best size
set.seed(123)
best_size.Decision = 2
prune.R1.Decision = prune.misclass(tree.R1.Decision , best = best_size.Decision)

#Summary of pruning result
summary(prune.R1.Decision)

#Text visualization
prune.R1.Decision

#Tree visualization
par(mfrow=c(1,1))
plot(prune.R1.Decision)
text(prune.R1.Decision ,col=1, cex=1/2)

#Predict using pruned tree model
set.seed(123)
tree_pred.Decision = predict(prune.R1.Decision, Data.R1.test[,-2], type="class")

#Generate the confusion matrix showing counts for pruned tree.
print("confusion matrix showing test counts")
per_pruned.Decision <- rattle::errorMatrix(Data.R1.test$Decision, tree_pred.Decision, count=FALSE)
per_pruned.Decision

#Overall error
print("overall error percentage")
cat(100-sum(diag(per_pruned.Decision), na.rm=TRUE))
