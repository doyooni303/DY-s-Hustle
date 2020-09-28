install.packages("pROC")
install.packages("ROCR")
install.packages("knitr")
install.packages("Epi")
library(Epi)

library(knitr)

#1,2

adm <- read.csv("Admission_Predict.csv")
adm<-adm[,-1]

input_idx <- c(1:7)
target_idx <- 8


adm_input<-adm[,input_idx]
adm_target<-adm[,target_idx]
nadm <- nrow(adm_input)#행의 수 카운트
nvar <- ncol(adm_input)#변수의 개수 카운트

matrix_adm<-matrix(0,ncol = 4, nrow=nvar )


for(i in 1:(ncol(adm_input))){
  m <- mean(adm_input[,i])
  std <- sqrt(var(adm_input[,i]))
  s<-skewness(adm_input[,i])
  k<-kurtosis(adm_input[,i])
  
  matrix_adm[i,]<-c(m,std,s,k)
  
}



rownames(matrix_adm) <- c(colnames(adm_input))
colnames(matrix_adm) <- c("Mean", "Std", "Skewness","Kurtosis")

matrix_adm


boxplot(adm, main="Boxplot of Data",xlab="Input Variables")


#3
summary(adm_input)
outlier_cutoff<-matrix(0,ncol=2,nrow=ncol(adm_input))
for (i in 1:ncol(adm_input)){
  
  UpperQ <- fivenum(adm_input[,i])[4]
  LowerQ <- fivenum(adm_input[,i])[2]
  iqr<-IQR(adm_input[,i],na.rm=F,type=7)
  outlier_cutoff[i,]<-c(LowerQ-iqr*1.5,UpperQ+iqr*1.5)
}
rownames(outlier_cutoff) <- c(colnames(adm_input))
colnames(outlier_cutoff) <- c("Lower bound", "Upper bound")


outlier_cutoff<-data.frame(outlier_cutoff)
outlier_cutoff


trimmed_index<-NULL
trimmed_index<-which(adm_input[,1]>=outlier_cutoff[1,1] & adm_input[,1]<=outlier_cutoff[1,2])
trimmed_index
trimmed_index<-which(adm_input[,2]>=outlier_cutoff[2,1] & adm_input[,2]<=outlier_cutoff[2,2])
trimmed_index
trimmed_index<-which(adm_input[,3]>=outlier_cutoff[3,1] & adm_input[,3]<=outlier_cutoff[3,2])
trimmed_index
trimmed_index<-which(adm_input[,4]>=outlier_cutoff[4,1] & adm_input[,4]<=outlier_cutoff[4,2])
trimmed_index
trimmed_index<-which(adm_input[,5]>=outlier_cutoff[5,1] & adm_input[,5]<=outlier_cutoff[5,2])
trimmed_index
trimmed_index<-which(adm_input[,6]>=outlier_cutoff[6,1] & adm_input[,6]<=outlier_cutoff[6,2])
trimmed_index
trimmed_index<-which(adm_input[,7]>=outlier_cutoff[7,1] & adm_input[,7]<=outlier_cutoff[7,2])
trimmed_index


#4
plot(adm_input, main="Scatter plot of Input variables")
corr_adm_input<-cor(adm_input)
corr_adm_input
corrplot(corr_adm_input,method = "number",main="Correlation plot of Input variables")


#5
adm_target[which(adm_target >0.8)] <- 1
adm_target[which(adm_target <= 0.8)] <- 0
adm_target

adm_data<-data.frame(adm_input,adm_target)

set.seed(12345)
trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
adm_trn <- adm_data[trn_idx,]
adm_tst <- adm_data[-trn_idx,]

full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
summary(full_lr)


#6
lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
lr_target <- adm_tst$adm_target #실제 calss
lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
lr_predicted[which(lr_response > 0.8)] <- 1
cm_full <- table(lr_target, lr_predicted)
cm_full

perf_mat <- matrix(0, 1, 6)
colnames(perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

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

perf_mat[1,] <- perf_eval2(cm_full)
perf_mat




#7
#roc_1
roc_point<- matrix(0, 20, 2)
colnames(roc_point) <- c("FPR", "TPR")
roc_eval<-function(cm){
  if(ncol(cm)==2){
    TPR <- cm[2,2]/sum(cm[2,])
    FPR <- cm[1,2]/sum(cm[1,])
    return(c(FPR,TPR))
    }
 else if(ncol(cm)==1){
   TPR <- 1
   FPR <- 1
    return(c(FPR,TPR))
  }
}


for(i in 1:length(roc_criteria)){

  adm_input<-adm[,input_idx]
  adm_target<-adm[,target_idx]
  nadm <- nrow(adm_input)#행의 수 카운트
  nvar <- ncol(adm_input)#변수의 개수 카운트
  adm_target
  roc_criteria<-seq(from = min(adm_target), to =1, by = 1/19) #길이 20
  adm_target[which(adm_target >roc_criteria[i])] <- 1
  adm_target[which(adm_target <= roc_criteria[i])] <- 0
  adm_data<-data.frame(adm_input,adm_target)
  
  set.seed(123)
  trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
  adm_trn <- adm_data[trn_idx,]
  adm_tst <- adm_data[-trn_idx,]
  
  full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
  
  lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
  lr_response
  lr_target <- adm_tst$adm_target #실제 calss
  lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
  lr_predicted[which(lr_response > roc_criteria[i])] <- 1
  lr_predicted
  cm_roc <- table(lr_target, lr_predicted)
  cm_roc
  roc_point[i,]<-roc_eval(cm_roc)

}

  
roc_point <- data.frame(roc_point)
roc_point<-roc_point[c(order(roc_point$FPR,roc_point$TPR)),]
rownames(roc_point)<-NULL
roc_point
plot(roc_point,type="s",xlim=c(0,1),main="ROC curve_1")

abline(a=0,b=1,col="black",lty=6)


#roc_2
roc_point<- matrix(0, 20, 2)
colnames(roc_point) <- c("FPR", "TPR")
roc_eval<-function(cm){
  if(ncol(cm)==2){
    TPR <- cm[2,2]/sum(cm[2,])
    FPR <- cm[1,2]/sum(cm[1,])
    return(c(FPR,TPR))
  }
  else if(ncol(cm)==1){
    TPR <- 1
    FPR <- 1
    return(c(FPR,TPR))
  }
}

for(i in 1:length(roc_criteria)){
  
  adm_input<-adm[,input_idx]
  adm_target<-adm[,target_idx]
  nadm <- nrow(adm_input)#행의 수 카운트
  nvar <- ncol(adm_input)#변수의 개수 카운트
  adm_target
  roc_criteria<-seq(from = min(adm_target), to =1, by = 1/19) #길이 20
  adm_target[which(adm_target >roc_criteria[i])] <- 1
  adm_target[which(adm_target <= roc_criteria[i])] <- 0
  adm_data<-data.frame(adm_input,adm_target)
  
  set.seed(10)
  trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
  adm_trn <- adm_data[trn_idx,]
  adm_tst <- adm_data[-trn_idx,]
  
  full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
  
  lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
  lr_response
  lr_target <- adm_tst$adm_target #실제 calss
  lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
  lr_predicted[which(lr_response > roc_criteria[i])] <- 1
  lr_predicted
  cm_roc <- table(lr_target, lr_predicted)
  cm_roc
  roc_point[i,]<-roc_eval(cm_roc)
}  



roc_point <- data.frame(roc_point)
roc_point<-roc_point[c(order(roc_point$FPR,roc_point$TPR)),]
rownames(roc_point)<-NULL
roc_point
plot(roc_point,type="s",xlim=c(0,1),main="ROC curve_2")
abline(a=0,b=1,col="black",lty=6)


#roc_3
roc_point<- matrix(0, 20, 2)
colnames(roc_point) <- c("FPR", "TPR")

roc_eval<-function(cm){
  if(ncol(cm)==2){
    TPR <- cm[2,2]/sum(cm[2,])
    FPR <- cm[1,2]/sum(cm[1,])
    return(c(FPR,TPR))
  }
  else if(ncol(cm)==1){
    TPR <- 1
    FPR <- 1
    return(c(FPR,TPR))
  }
}

for(i in 1:length(roc_criteria)){
  
  adm_input<-adm[,input_idx]
  adm_target<-adm[,target_idx]
  nadm <- nrow(adm_input)#행의 수 카운트
  nvar <- ncol(adm_input)#변수의 개수 카운트
  adm_target
  roc_criteria<-seq(from = min(adm_target), to =1, by = 1/19) #길이 20
  adm_target[which(adm_target >roc_criteria[i])] <- 1
  adm_target[which(adm_target <= roc_criteria[i])] <- 0
  adm_data<-data.frame(adm_input,adm_target)
  
  set.seed(1256)
  trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
  adm_trn <- adm_data[trn_idx,]
  adm_tst <- adm_data[-trn_idx,]
  
  full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
  
  lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
  lr_response
  lr_target <- adm_tst$adm_target #실제 calss
  lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
  lr_predicted[which(lr_response > roc_criteria[i])] <- 1
  lr_predicted
  cm_roc <- table(lr_target, lr_predicted)
  cm_roc
  roc_point[i,]<-roc_eval(cm_roc)
}
roc_point <- data.frame(roc_point)
roc_point<-roc_point[c(order(roc_point$FPR,roc_point$TPR)),]
rownames(roc_point)<-NULL
roc_point
plot(roc_point,type="s",xlim=c(0,1),main="ROC curve_3")
abline(a=0,b=1,col="black",lty=6)

#roc_4
roc_point<- matrix(0, 20, 2)
colnames(roc_point) <- c("FPR", "TPR")

roc_eval<-function(cm){
  if(ncol(cm)==2){
    TPR <- cm[2,2]/sum(cm[2,])
    FPR <- cm[1,2]/sum(cm[1,])
    return(c(FPR,TPR))
  }
  else if(ncol(cm)==1){
    TPR <- 1
    FPR <- 1
    return(c(FPR,TPR))
  }
}

for(i in 1:length(roc_criteria)){
  
  adm_input<-adm[,input_idx]
  adm_target<-adm[,target_idx]
  nadm <- nrow(adm_input)#행의 수 카운트
  nvar <- ncol(adm_input)#변수의 개수 카운트
  adm_target
  roc_criteria<-seq(from = min(adm_target), to =1, by = 1/19) #길이 20
  adm_target[which(adm_target >roc_criteria[i])] <- 1
  adm_target[which(adm_target <= roc_criteria[i])] <- 0
  adm_data<-data.frame(adm_input,adm_target)
  
  set.seed(55)
  trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
  adm_trn <- adm_data[trn_idx,]
  adm_tst <- adm_data[-trn_idx,]
  
  full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
  
  lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
  lr_response
  lr_target <- adm_tst$adm_target #실제 calss
  lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
  lr_predicted[which(lr_response > roc_criteria[i])] <- 1
  lr_predicted
  cm_roc <- table(lr_target, lr_predicted)
  cm_roc
  roc_point[i,]<-roc_eval(cm_roc)
}
roc_point <- data.frame(roc_point)
roc_point<-roc_point[c(order(roc_point$FPR,roc_point$TPR)),]
rownames(roc_point)<-NULL
roc_point
plot(roc_point,type="s",xlim=c(0,1),main="ROC curve_4")
abline(a=0,b=1,col="black",lty=6)

#roc_5
roc_point<- matrix(0, 20, 2)
colnames(roc_point) <- c("FPR", "TPR")

roc_eval<-function(cm){
  if(ncol(cm)==2){
    TPR <- cm[2,2]/sum(cm[2,])
    FPR <- cm[1,2]/sum(cm[1,])
    return(c(FPR,TPR))
  }
  else if(ncol(cm)==1){
    TPR <- 1
    FPR <- 1
    return(c(FPR,TPR))
  }
}

for(i in 1:length(roc_criteria)){
  
  adm_input<-adm[,input_idx]
  adm_target<-adm[,target_idx]
  nadm <- nrow(adm_input)#행의 수 카운트
  nvar <- ncol(adm_input)#변수의 개수 카운트
  adm_target
  roc_criteria<-seq(from = min(adm_target), to =1, by = 1/19) #길이 20
  adm_target[which(adm_target >roc_criteria[i])] <- 1
  adm_target[which(adm_target <= roc_criteria[i])] <- 0
  adm_data<-data.frame(adm_input,adm_target)
  
  set.seed(987)
  trn_idx <- sample(1:nrow(adm_data), round(0.7*nrow(adm_data)))
  adm_trn <- adm_data[trn_idx,]
  adm_tst <- adm_data[-trn_idx,]
  
  full_lr <- glm(adm_target ~ ., family=binomial, adm_trn)
  
  lr_response <- predict(full_lr, type = "response", newdata = adm_tst) # 예측 확률가
  lr_response
  lr_target <- adm_tst$adm_target #실제 calss
  lr_predicted <- rep(0, length(lr_target)) #예측 class로 변환
  lr_predicted[which(lr_response > roc_criteria[i])] <- 1
  lr_predicted
  cm_roc <- table(lr_target, lr_predicted)
  cm_roc
  roc_point[i,]<-roc_eval(cm_roc)
}
roc_point <- data.frame(roc_point)
roc_point<-roc_point[c(order(roc_point$FPR,roc_point$TPR)),]
rownames(roc_point)<-NULL
roc_point
plot(roc_point,type="s",xlim=c(0,1),main="ROC curve_5")
abline(a=0,b=1,col="black",lty=6)




#8
install.packages('mlbench')
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

bc <- bc[,-1]

# convert to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}


bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

# Prep Training and Test data.
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(12345)
trn_idx <- sample(1:nrow(bc), round(0.7*nrow(bc)))
bc_trn <- bc[trn_idx,]
bc_tst <- bc[-trn_idx,]


# Class distribution of train data
  table(bc_trn$Class)

logitmod <- glm(Class ~ ., family = "binomial", data=bc_trn)

summary(logitmod)

pred <- predict(logitmod, newdata = bc_tst, type = "response")
pred

bc_target <- bc_tst$Class
bc_predicted <- rep(0, length(bc_target)) #예측 class로 변환
bc_predicted[which(pred > 0.3)] <- 1

bc_cm <- table(bc_target, bc_predicted)
bc_cm

bc_perf_mat <- matrix(0, 1, 6)
colnames(bc_perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(bc_perf_mat) <- NULL
bc_perf_mat[1,] <- perf_eval2(bc_cm)
bc_perf_mat
