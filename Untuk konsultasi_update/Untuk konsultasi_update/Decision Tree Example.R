library(tree) #Load tree library to construct classification trees
library(ISLR) #Load library contains the Carseats data set. 
library(readxl) # Load library to read excel files

#Load the Carseats data
Dataset <- read_excel("Untuk konsultasi_update/Untuk konsultasi_update/4. Tabulasi buat statistik_kirim.xlsx",sheet = "All responds Q0 - Q41")
head(Dataset)

library(dplyr)

Carseats$Sales = sample(Carseats$Sales)
Carseats = Carseats %>%
  mutate(High = as.factor(ifelse(Sales <= threshold, "No", "Yes")))
head(Carseats)

attach(Carseats)

library(dplyr)
Carseats %>%
  dplyr::mutate(ShelveLoc=as.factor(ShelveLoc)) %>%
  GGally::ggpairs(columns=c(1,4),
                  mapping=ggplot2::aes(colour=ShelveLoc, alpha=0.5),
                  diag=list(continuous="density",
                            discrete="bar"),
                  upper=list(continuous="cor",
                             combo="box",
                             discrete="ratio"),
                  lower=list(continuous="points",
                             combo="denstrip",
                             discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())


Carseats %>%
  dplyr::mutate(High=as.factor(High)) %>%
  GGally::ggpairs(columns=c(2,6),
                  mapping=ggplot2::aes(colour=High, alpha=0.5),
                  diag=list(continuous="density",
                            discrete="bar"),
                  upper=list(continuous="cor",
                             combo="box",
                             discrete="ratio"),
                  lower=list(continuous="points",
                             combo="denstrip",
                             discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())

#Dataset for training
n = runif(1, 200, 300)
train=sample (1: nrow(Carseats ), n)  #Row sample for training data
head(train)

#Dataset for testing
Carseats.test = Carseats [-train ,]

set.seed(123)
tree.carseats = tree(High ~ .-Sales, Carseats, subset = train )

#Summarize the classification tree model
summary(tree.carseats)

#Text visualization of model
tree.carseats

#Tree visualization of model
plot(tree.carseats )
text(tree.carseats ,cex=1/2)

#Predict the model using data testing Carseats.test
set.seed(123)
tree.pred=predict(tree.carseats , Carseats.test, type ="class")
head(tree.pred, 5)

#Generate the confusion matrix showing proportions from testing data .
print("confusion matrix showing test proportion")

per <- rattle::errorMatrix(Carseats.test$High, tree.pred, count = FALSE)
per

#Calculate the testing overall error percentage.
print("overall error percentage")

cat(100-sum(diag(per), na.rm=TRUE))

print("overall error percentage")

cat(100-sum(diag(per), na.rm=TRUE))

#Calculate deviance
set.seed (123)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass, K = 10)
cv.carseats

#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

#Prune the best size
set.seed(123)
best_size = 5
prune.carseats = prune.misclass(tree.carseats , best = best_size)

#Summary of pruning result
summary(prune.carseats)

#Text visualization
prune.carseats

#Tree visualization
plot(prune.carseats )
text(prune.carseats ,col=1, cex=1/2)

#Predict using pruned tree model
set.seed(123)
tree_pred = predict(prune.carseats, Carseats.test, type="class")

#Generate the confusion matrix showing counts for pruned tree.
print("confusion matrix showing test counts")

per_pruned <- rattle::errorMatrix(Carseats.test$High, tree_pred, count=FALSE)
per_pruned

#Overall error
print("overall error percentage")

cat(100-sum(diag(per_pruned), na.rm=TRUE))

# Create a data frame with 400 rows
newdata <- data.frame(
  CompPrice = rep(115, 400),
  Income = rep(28, 400),
  Advertising = rep(11, 400),
  Population = rep(29, 400),
  Price = rep(86, 400),
  ShelveLoc = rep("Good", 400),
  Age = rep(53, 400),
  Education = rep(18, 400),
  Urban = rep("Yes", 400),
  US = rep("Yes", 400)
)
newdata$ShelveLoc<-as.factor(newdata$ShelveLoc)
newdata$Urban<-as.factor(newdata$Urban)
newdata$US<-as.factor(newdata$US)
# Predict using the new data
set.seed(123)
# Predict using unpruned tree model
unpruned_pred <- predict(tree.carseats, newdata, type="class")

# Predict using pruned tree model
pruned_pred <- predict(prune.carseats, newdata, type="class")

# Print the predictions
unpruned_pred_output<-cat("Unpruned tree prediction: ", unpruned_pred, "\n")

unpruned_pred_output<-cat("Pruned tree prediction: ", pruned_pred)

summary(unpruned_pred)

summary(pruned_pred)
