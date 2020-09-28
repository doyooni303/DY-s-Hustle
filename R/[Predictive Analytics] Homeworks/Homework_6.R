install.packages("caret")
install.packages("e1071")
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
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
rownames(Perf.Table) <- c("CART")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

wine<-read.csv("wine.csv")
input_idx <- c(2:14)
target_idx <- 1
wine_input <- wine[,input_idx]
wine_target <- as.factor(wine[,target_idx])
wine_data<-data.frame(wine_input,Type=wine_target)
head(wine_data)

trn_idx<-sample(1:nrow(wine),round(0.7*nrow(wine)))

set.seed(1234)
wine_trn <- data.frame(wine_input[trn_idx,], Type = wine_target[trn_idx])
wine_tst <- data.frame(wine_input[-trn_idx,], Type = wine_target[-trn_idx])

wine_full_model <- tree(Type ~ ., wine_data)
summary(wine_full_model)


plot(wine_full_model, main="Decision Tree")
text(wine_full_model, pretty = 1)
full_pre_y <- predict(wine_full_model, wine_data, type = "class")
full_cfm <- table(wine_target, full_pre_y)
full_cfm


trn_model<-tree(Type ~ ., wine_trn)
summary(trn_model)
plot(trn_model)
text(trn_model, pretty = 1)

test_pre_y <- predict(trn_model, wine_tst, type = "class")
confusionMatrix(wine_tst$Type,test_pre_y)

test_cfm <- table(wine_tst$Type, test_pre_y)
test_cfm


gini_model<-tree(Type ~ ., wine_data, split = "gini")
summary(gini_model)
plot(gini_model, main="good")
text(gini_model, cex=0.6)
gini_pre_y <- predict(gini_model, wine_data, type = "class")
gini_cfm <- table(wine_target, gini_pre_y)
gini_cfm
confusionMatrix(wine_target,gini_pre_y)
confusionMatrix(wine_target,full_pre_y)
confusionMatrix(wine_tst$Type,test_pre_y)
