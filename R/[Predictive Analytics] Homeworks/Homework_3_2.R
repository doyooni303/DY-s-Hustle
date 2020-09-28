install.packages("fastDummies")

loan=read.csv("loan_data.csv")

loan_x=loan[,1:22] #[:-23]µµ °°Àº ¾ê±â
loan_target=as.factor(loan[,23])
loan_data=data.frame(loan_x,loan_target)
loan_data <- rename(loan_data,"TARGET"="loan_target")

table(loan_data$TARGET)
levels(loan_data$SEX)
levels(loan_data$PAY_METHOD)
levels(loan_data$JOB)
trn_idx <- sample(1:nrow(loan_data), round(0.7*nrow(loan_data)))
trn_idx


#1 without scaling / binomialize categorical variables 
train1<-loan_data[trn_idx,]
test1<-loan_data[-trn_idx,]

loan_ds = fastDummies::dummy_cols(loan_data,select_columns = c('SEX','PAY_METHOD','JOB'),
                                  remove_first_dummy = F)
model1 <- glm(TARGET ~ ., family=binomial, train1)

summary(model1)
#AGE, TOT_LOAN, TOT_LOAN_CRD,LOAN_crd-cnt,late-rate,ins,calltime, telcost,sex, job remove 

lr_response1 <- predict(model1, type = "response", newdata = test1)
lr_target1 <- test1$TARGET
lr_predicted1 <- rep(0, length(lr_target1))
lr_predicted1[which(lr_response1 >= 0.5)] <- 1
cm_full1 <- table(lr_target1, lr_predicted1)
cm_full1

perf_eval1 <- function(cm){
  
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

perf_mat1 <- matrix(0, 1, 6)
colnames(perf_mat1) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat1) <- "Logstic Regression"


perf_mat1[1,] <- perf_eval1(cm_full1)
perf_mat1


#2 wtithout scaling / binoialize categorical variables / pvalue criteria
train2<-loan_data[trn_idx,]
test2<-loan_data[-trn_idx,]
model2<-glm(TARGET~ LOAN_BNK+	LOAN_CPT+	CRDT_CNT+	GUARN_CNT+	INCOME+	LATE_RATE_1Y+	CANCEL_CNT_1Y	+MOBILE_PRICE+	SUSP_DAY+	LATE_TEL+	COMB_COMM+PAY_METHOD
, family = binomial, train2)

summary(model2)


lr_response2 <- predict(model2, type = "response", newdata = test2)
lr_target2 <- test2$TARGET
lr_predicted2 <- rep(0, length(lr_target2))
lr_predicted2[which(lr_response2 >= 0.5)] <- 1
cm_full2 <- table(lr_target2, lr_predicted2)
cm_full2

perf_eval2 <- function(cm){
  
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

perf_mat2 <- matrix(0, 1, 6)
colnames(perf_mat2) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat2) <- "Logstic Regression"


perf_mat2[1,] <- perf_eval2(cm_full2)
perf_mat2


#3 with scaling / binomialize categorical variables
tempotrain<-loan_data[trn_idx,]
tempotest<-loan_data[-trn_idx,]


normtrain <- scale(tempotrain[,1:19], center = TRUE, scale = TRUE)
train3<-data.frame(normtrain,tempotrain[,20:23])
normtest<- scale(tempotest[,1:19],center=T,scale=T)
test3<-data.frame(normtest,tempotest[,20:23])
model3<-glm(TARGET~ LOAN_BNK+	LOAN_CPT+	CRDT_CNT+	GUARN_CNT+	INCOME+	LATE_RATE_1Y+	CANCEL_CNT_1Y	+MOBILE_PRICE+	SUSP_DAY+	LATE_TEL+	COMB_COMM+PAY_METHOD
            , family = binomial, train3)
summary(model3)

#cutoff=0.5

lr_response3 <- predict(model3, type = "response", newdata = test3)
lr_target3 <- test3$TARGET
lr_predicted3 <- rep(0, length(lr_target3))
lr_predicted3[which(lr_response3 >= 0.5)] <- 1
cm_full3 <- table(lr_target3,lr_predicted3)
cm_full3


perf_eval3 <- function(cm){
  
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

perf_mat3 <- matrix(0, 1, 6)
colnames(perf_mat3) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat3) <- "Logstic Regression"


perf_mat3[1,] <- perf_eval3(cm_full3)
perf_mat3


# cutoff = 0.25

lr_response4 <- predict(model3, type = "response", newdata = test3)
lr_target4 <- test3$TARGET
lr_predicted4 <- rep(0, length(lr_target4))
lr_predicted4[which(lr_response4 >= 0.25)] <- 1
cm_full4 <- table(lr_target4,lr_predicted4)
cm_full4


perf_eval4 <- function(cm){
  
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

perf_mat4 <- matrix(0, 1, 6)
colnames(perf_mat4) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat4) <- "Logstic Regression"


perf_mat4[1,] <- perf_eval4(cm_full4)
perf_mat4


#cutoff=0.1

lr_response5 <- predict(model3, type = "response", newdata = test3)
lr_target5 <- test3$TARGET
lr_predicted5 <- rep(0, length(lr_target5))
lr_predicted5[which(lr_response5 >= 0.1)] <- 1
cm_full5 <- table(lr_target5,lr_predicted5)
cm_full5


perf_eval5 <- function(cm){
  
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

perf_mat5 <- matrix(0, 1, 6)
colnames(perf_mat5) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat5) <- "Logstic Regression"


perf_mat5[1,] <- perf_eval5(cm_full5)
perf_mat5
