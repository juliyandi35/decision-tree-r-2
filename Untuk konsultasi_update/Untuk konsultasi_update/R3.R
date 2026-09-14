# What are the influencing factors for batik consumers to buy traditional batik products in the future?
library(tree) #Load tree library to construct classification trees
library(readxl)
Data.R3 <- read_excel("R3.xlsx")
head(Data.R3)

Data.R3 <- na.omit(Data.R3)
head(Data.R3)
str(Data.R3)

# Membuat urutan peringkat
# Option 1
freq1 <- table(Data.R3$Option1)
ranking1 <- sort(rank(freq1),decreasing = FALSE)
ranking1

# Option 2
freq2 <- table(Data.R3$Option2)
ranking2 <- sort(rank(freq2),decreasing = FALSE)
ranking2

# Overall
freq_all <- table(rbind(Data.R3$Option1,Data.R3$Option2))
ranking_all <- sort(rank(freq_all),decreasing = FALSE)
ranking_all

# Rank Consider Q25
Rank.Consider.25 <- read_excel("R3.xlsx",sheet = "Rank_Consider_No 25")
head(Rank.Consider.25)
Rank.Consider.25 <- data.frame(Rank.Consider.25[,-c(1:2)])
summary(Rank.Consider.25)
Rank.Q25 <- data.frame(Overall.design = mean(Rank.Consider.25$Overall.design, na.rm = TRUE),
                       Colour = mean(Rank.Consider.25$Colour, na.rm = TRUE),
                       Motif = mean(Rank.Consider.25$Motifs, na.rm = TRUE),
                       Dye.Type = mean(Rank.Consider.25$Type.of.dyes, na.rm = TRUE),
                       Fabric.Material = mean(Rank.Consider.25$Fabric.material, na.rm = TRUE),
                       Making.technique = mean(Rank.Consider.25$Making.technique, na.rm = TRUE),
                       Price = mean(Rank.Consider.25$Price, na.rm = TRUE))
Rank.Q25
sort(rank(Rank.Q25),decreasing = TRUE) # Semakin besar angka rank, semakin tinggi peringkatnya.

# Rank Consider Q26
Rank.Consider.26 <- read_excel("R3.xlsx",sheet = "Rank_Consider_No 26")
head(Rank.Consider.26)
Rank.Consider.26 <- data.frame(Rank.Consider.26[,-c(1:2)])
summary(Rank.Consider.26)
Rank.Q26 <- data.frame(Appearance = mean(Rank.Consider.26$Appearance, na.rm = TRUE),
                       Comfortability = mean(Rank.Consider.26$Comfortability, na.rm = TRUE),
                       Practicality = mean(Rank.Consider.26$Practicality, na.rm = TRUE),
                       Durability = mean(Rank.Consider.26$Durability, na.rm = TRUE)
                       )
Rank.Q26
sort(rank(Rank.Q26),decreasing = TRUE) # Semakin besar angka rank, semakin tinggi peringkatnya.

# Decision Tree Analysis
Data.R3$Buy_Freq <- factor(Data.R3$Buy_Freq)
Data.R3$Option1 <- factor(Data.R3$Option1)
Data.R3$Option2 <- factor(Data.R3$Option2)

str(Data.R3)
Data.R3 <- data.frame(Data.R3)

# Split data
set.seed(123)
n = runif(1, 100, 200)
train=sample(1:nrow(Data.R3), n)  #Row sample for training data
head(train)

#Dataset for testing
Data.R3.test = Data.R3[-train ,]

tree.R3 = tree(Buy_Freq~., Data.R3, subset = train)

#Summarize the classification tree model
summary(tree.R3)

#Text visualization of model
tree.R3

#Tree visualization of model
plot(tree.R3)
text(tree.R3,cex=1/2)

#Predict the model using data testing Carseats.test
set.seed(123)
tree.pred=predict(tree.R3,Data.R3.test, type ="class")
head(tree.pred, 5)

#Generate the confusion matrix showing proportions from testing data .
print("confusion matrix showing test proportion")
per <- rattle::errorMatrix(Data.R3.test$Buy_Freq, tree.pred, count = FALSE)
per

#Calculate the testing overall error percentage.
print("overall error percentage")
cat(100-sum(diag(per), na.rm=TRUE))

#Calculate deviance
set.seed (123)
cv.R3 = cv.tree(tree.R3, FUN=prune.misclass, K = 10)
cv.R3

#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.R3$size ,cv.R3$dev ,type="b")
plot(cv.R3$k ,cv.R3$dev ,type="b")

#Prune the best size
set.seed(123)
best_size = 2
prune.R3 = prune.misclass(tree.R3 , best = best_size)

#Summary of pruning result
summary(prune.R3)

#Text visualization
prune.R3

#Tree visualization
par(mfrow=c(1,1))
plot(prune.R3 )
text(prune.R3 ,col=1, cex=1/2)

#Predict using pruned tree model
set.seed(123)
tree_pred = predict(prune.R3, Data.R3.test, type="class")

#Generate the confusion matrix showing counts for pruned tree.
print("confusion matrix showing test counts")
per_pruned <- rattle::errorMatrix(Data.R3.test$Buy_Freq, tree_pred, count=FALSE)
per_pruned

#Overall error
print("overall error percentage")
cat(100-sum(diag(per_pruned), na.rm=TRUE))
