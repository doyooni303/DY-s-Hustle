perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Performance table
heart.Table <- matrix(0, nrow = 6, ncol = 6)
rownames(heart.Table) <- c("full","pruned","rpart","rpart.pruned","party","evtree")
colnames(heart.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

heart <- read.csv("heart.csv")
input.idx <- c(1:13)
target.idx <- 14

heart.input <- heart[,input.idx]
heart.target <- as.factor(heart[,target.idx])

heart.data <- data.frame(heart.input, target=heart.target)

trn.idx <- sample(1:nrow(heart.data), 200)
val.idx <- -trn.idx

heart.trn<-heart.data[trn.idx,]
heart.val<-heart.data[val.idx,]

#[Q1]
full.model <- tree(target ~ ., heart.trn)
plot(full.model, main="Full Model")
text(full.model, pretty = 1)
summary(full.model)

full.prey <- predict(full.model, heart.val, type = "class")
full.cfm <- table(heart.val$target, full.prey)
full.cfm

heart.Table[1,] <- perf_eval(full.cfm)
heart.Table


#[Q2]
full.model.cv <- cv.tree(full.model, FUN = prune.misclass)
plot(full.model.cv$size, full.model.cv$dev, type = "b")

pruned.model <- prune.misclass(full.model, best = 6)
plot(pruned.model)
text(pruned.model, pretty = 1)

summary(pruned.model)

pruned.prey <- predict(pruned.model, heart.val, type = "class")
pruned.cfm <- table(heart.val$target, pruned.prey)
pruned.cfm

heart.Table[2,] <- perf_eval(pruned.cfm)
heart.Table

#[Q3]
#1)rpart
install.packages("rpart.plot")
library("rpart.plot")


rpart.model <- rpart(target~., data = heart.trn, method = 'class')
summary(rpart.model)
rpart.plot(rpart.model, extra = "auto")
rpart.model
rpart.prey <- predict(rpart.model, heart.val, type = "class")
rpart.cfm <- table(heart.val$target, rpart.prey)
rpart.cfm

heart.Table[3,] <- perf_eval(rpart.cfm)
heart.Table

printcp(rpart.model)
plotcp(rpart.model)

srpart.pruned.model<-prune(rpart.model, cp= rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),"CP"])
plot(rpart.pruned.model)
text(rpart.pruned.model)

rpart.pruned.prey <- predict(rpart.pruned.model, heart.val, type = "class")
rpart.pruned.cfm <- table(heart.val$target, rpart.pruned.prey)
rpart.pruned.cfm

heart.Table[4,] <- perf_eval(rpart.pruned.cfm)
heart.Table
rownames(heart.Table)[4]<-c("rpart.pruned")
heart.Table

#2) PARTY
install.packages("party")
library(party)
party.model<-ctree(target~., data=heart.trn)
plot(party.model)
#별도의 pruning 과정이 없어도 된다고 함.

party.prey <- predict(party.model, heart.val, type = "response")
party.cfm <- table(heart.val$target, party.prey)
party.cfm

heart.Table[5,] <- perf_eval(party.cfm)
heart.Table


#3) evtree
install.packages("evtree")
library(evtree)
ev.model<- evtree(target~., data=heart.trn)
summary(ev.model)
plot(ev.model)

ev.prey <- predict(ev.model, heart.val, type = "response")
ev.cfm <- table(heart.val$target, ev.prey)
ev.cfm

heart.Table[6,] <- perf_eval(ev.cfm)
heart.Table

data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
cpus.ltr
summary(cpus.ltr)
plot(cpus.ltr);  text(cpus.ltr)
