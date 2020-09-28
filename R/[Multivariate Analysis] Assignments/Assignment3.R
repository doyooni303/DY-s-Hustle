library(moments)
library(corrplot)

#2
kc <- read.csv("kc_house_data.csv")
kc<-kc[,-(1:2)]#id,date deleted
kc<-house[,-15]#zipcode deleted
nHouse <- nrow(kc)#행의 수 카운트
nVar <- ncol(kc)#변수의 개수 카운트

boxplot(kc, main="Boxplot of Data")


matrix_kc_sv<-matrix(0,ncol = 4, nrow= ncol(kc))
kc_resid <- resid(mlr_kc)


for(i in 1:(ncol(kc))){
  m <- mean(kc[,i])
  std <- sqrt(var(kc[,i]))
  s<-skewness(kc[,i])
  k<-kurtosis(kc[,i])
  
  matrix_kc_sv[i,]<-c(m,std,s,k)
  
}

rownames(matrix_kc_sv) <- c(colnames(kc))
colnames(matrix_kc_sv) <- c("Mean", "Std", "Skewness","Kurtosis")

matrix_kc_sv


#Q3
summary(kc)

outlier_cutoff<-matrix(0,ncol=2,nrow=ncol(kc))
for (i in 1:ncol(kc)){
  
  UpperQ <- fivenum(kc[,i])[4]
  LowerQ <- fivenum(kc[,i])[2]
  iqr<-IQR(kc[,i],na.rm=F,type=7)
  outlier_cutoff[i,]<-c(LowerQ-iqr*1.5,UpperQ+iqr*1.5)
}
rownames(outlier_cutoff) <- c(colnames(kc))
colnames(outlier_cutoff) <- c("Lower bound", "Upper bound")


outlier_cutoff<-data.frame(outlier_cutoff)

trimmed_kc<-subset(kc,sqft_lot>=outlier_cutoff[5,1] & sqft_lot<=outlier_cutoff[5,2]) 
trimmed_kc<-subset(trimmed_kc,sqft_lot15>=outlier_cutoff[17,1] & sqft_lot15<=outlier_cutoff[17,2])


#Q4
iv_trimmed_kc<-trimmed_kc[,-1]
plot(iv_trimmed_kc)
cor_kc<-cor(iv_trimmed_kc)
cor_kc
corrplot(cor_kc,method="number")
corrplot(cor_kc,method="square")


#Q5
set.seed(1234)
nKc <- nrow(trimmed_kc)#행의 수 카운트
nVar <- ncol(trimmed_kc)#변수의 개수 카운트
kc_trn_idx <- sample(1:nKc, round(0.7*nKc))
kc_trn_data <- trimmed_kc[kc_trn_idx,]
kc_tst_data <- trimmed_kc[-kc_trn_idx,]

mlr_kc <- lm(price ~ ., data = trimmed_kc)
mlr_kc
summary(mlr_kc)
plot(mlr_kc)


#Q6

#Q7
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 1, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("KC House")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat


mlr_test <- predict(mlr_kc, newdata = kc_tst_data)

perf_mat[1,] <- perf_eval_reg(kc_tst_data$price, mlr_test)
perf_mat


#Q9
set.seed(1234)
new_kc<-data.frame(trimmed_kc$price,trimmed_kc$sqft_lot,trimmed_kc$waterfront,trimmed_kc$view,trimmed_kc$yr_built,trimmed_kc$long,trimmed_kc$sqft_living15,trimmed_kc$sqft_lot15)
colnames(new_kc) <- c("price","sqft_lot", "waterfront", "view","yr_built", "long", "sqft_living15","sqft_lot15" )
nKc <- nrow(new_kc)#행의 수 카운트
nVar <- ncol(new_kc)#변수의 개수 카운트

kc_trn_idx <- sample(1:nKc, round(0.7*nKc))
new_kc_trn_data <- new_kc[kc_trn_idx,]
new_kc_tst_data <- new_kc[-kc_trn_idx,]
mlr_kc_new <- lm(price ~ ., data = new_kc)
summary(mlr_kc_new)
plot(mlr_kc)


new_perf_mat <- matrix(0, nrow = 1, ncol = 3)

# Initialize a performance summary
rownames(new_perf_mat) <- c("KC House_new")
colnames(new_perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat


mlr_test <- predict(mlr_kc_new, newdata = new_kc_tst_data)

new_perf_mat[1,] <- perf_eval_reg(new_kc_tst_data$price, mlr_test)
new_perf_mat

# normality test of residuals
kc_resid <- resid(mlr_kc_new)

m <- mean(kc_resid)
std <- sqrt(var(kc_resid))

hist(kc_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

skewness(kc_resid)
kurtosis(kc_resid)

plot(mlr_kc_new)
