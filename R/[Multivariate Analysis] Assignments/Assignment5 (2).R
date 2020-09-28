
# Performance evaluation function for regression --------------------------함수정의
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
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
rownames(perf_mat)<-c("full_mlr")
perf_mat


weather<-read.csv("Weather_Ankara.csv")


# data setting
input_idx<-c(1:9)
output_idx<-10

weather_input<-weather[,input_idx]
weather_output<-weather[,output_idx]

weather_input<-scale(weather_input,center=T,scale=T)
weather_output<-scale(weather_output,center=T,scale=T)
weather_data<-data.frame(weather_input,Mean_temperature=weather_output)

set.seed(1234)
trn_idx<-sample(1:nrow(weather_data), 250)
vld_idx<-c(-trn_idx)

weather_trn<-weather_data[trn_idx,]
weather_vld<-weather_data[vld_idx,]


#1
full_mlr <- lm(Mean_temperature ~ ., data = weather_trn)
summary(full_mlr)

prdt_full_mlr <- predict(full_mlr, newdata = weather_vld)
perf_mat[1,] <- perf_eval_reg(weather_vld$Mean_temperature, prdt_full_mlr)
perf_mat


#2
install.packages("combinat")
nvar<-NULL
num<-c(1:9)
seed<-c(1:511)

# 조합생성 
c1<-as.array(combn(num,1))#9
c2<-as.array(combn(num,2))#36
c3<-as.array(combn(num,3))#84
c4<-as.array(combn(num,4))#126
c5<-as.array(combn(num,5))#126
c6<-as.array(combn(num,6))#84
c7<-as.array(combn(num,7))#36
c8<-as.array(combn(num,8))#9
c9<-num #1
c1[3]

#c1
c1_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c1))


start_time <- proc.time()
for(i in 1: ncol(c1)){
   exh_input<-weather_input[,c1[i]]
  names(exh_input)<-colnames(weather_input)[c1[i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  colnames(exh_data)<-c(colnames(weather_input)[c1[i]],"Mean_temperature")
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-1-1))/(tss/(length(trn_idx)-1))

  c1_adjRsquared_Table[i]<-adjrsq
  }
end_time <- proc.time()
end_time - start_time

#c2
c2_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c2))
start_time <- proc.time()
for(i in 1: ncol(c2)){
  exh_input<-weather_input[,c2[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c2_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c3
c3_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c3))
start_time <- proc.time()
for(i in 1: ncol(c3)){
  exh_input<-weather_input[,c3[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c3_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c4
c4_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c4))
start_time <- proc.time()
for(i in 1: ncol(c4)){
  exh_input<-weather_input[,c4[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c4_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c5
c5_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c5))
start_time <- proc.time()
for(i in 1: ncol(c5)){
  exh_input<-weather_input[,c5[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c5_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c6
c6_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c6))
start_time <- proc.time()
for(i in 1: ncol(c6)){
  exh_input<-weather_input[,c6[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c6_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c7
c7_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c7))
start_time <- proc.time()
for(i in 1: ncol(c7)){
  exh_input<-weather_input[,c7[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c7_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time

#c8
c8_adjRsquared_Table <- matrix(0, nrow = 1, ncol = ncol(c8))
start_time <- proc.time()
for(i in 1: ncol(c8)){
  exh_input<-weather_input[,c8[,i]]
  exh_output<-weather_output
  exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
  
  exh_trn<-exh_data[trn_idx,]
  exh_vld<-exh_data[vld_idx,]
  
  exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
  trn_pre<-predict(exh_mlr, newdata = exh_trn)
  
  rss <- sum((trn_pre - exh_trn$Mean_temperature) ^ 2)  ## residual sum of squares
  tss <- sum((exh_trn$Mean_temperature - mean(exh_trn$Mean_temperature)) ^ 2)  ## total sum of squares
  adjrsq <- 1 - (rss/(length(trn_idx)-ncol(exh_input)-1))/(tss/(length(trn_idx)-1))
  
  c8_adjRsquared_Table[i]<-adjrsq
}
end_time <- proc.time()
end_time - start_time


exh_mat <- matrix(0, nrow = 9, ncol = 3)
colnames(exh_mat) <- c("RMSE", "MAE", "MAPE")

#c1
c1max<-which.max(c1_adjRsquared_Table)
exh_input<-weather_input[,c1[,c1max]]
names(exh_input)<-colnames(weather_input)[c1[c1max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c1[,c1max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c1[,c1max]]
exh_mat[1,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat

#c2
c2max<-which.max(c2_adjRsquared_Table)

exh_input<-weather_input[,c2[,c2max]]

exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c2[,c2max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)

exh_mat[2,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c2[,c2max]]



#3
c3max<-which.max(c3_adjRsquared_Table)
exh_input<-weather_input[,c3[,c3max]]
names(exh_input)<-colnames(weather_input)[c3[,c3max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c3[,c3max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c1[,c1max]]
exh_mat[3,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat

colnames(weather_input)[c3[,c3max]]

#4
c4max<-which.max(c4_adjRsquared_Table)
exh_input<-weather_input[,c4[,c4max]]
names(exh_input)<-colnames(weather_input)[c4[,c4max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c4[,c4max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c4[,c4max]]
exh_mat[4,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c4[,c4max]]

#c5
c5max<-which.max(c5_adjRsquared_Table)
exh_input<-weather_input[,c5[,c5max]]
names(exh_input)<-colnames(weather_input)[c5[,c5max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c5[,c5max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c5[,c5max]]
exh_mat[5,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c5[,c5max]]

#c6
c6max<-which.max(c6_adjRsquared_Table)
exh_input<-weather_input[,c6[,c6max]]
names(exh_input)<-colnames(weather_input)[c6[c6max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c6[,c6max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c6[,c6max]]
exh_mat[6,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c6[,c6max]]

#c7
c7max<-which.max(c7_adjRsquared_Table)
exh_input<-weather_input[,c7[,c7max]]
names(exh_input)<-colnames(weather_input)[c7[,c7max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c7[,c7max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c7[,c7max]]
exh_mat[7,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c7[,c7max]]

#c8
c8max<-which.max(c8_adjRsquared_Table)
exh_input<-weather_input[,c8[,c8max]]
names(exh_input)<-colnames(weather_input)[c8[,c8max]]
exh_output<-weather_output
exh_data<-data.frame(exh_input,Mean_temperature = exh_output)
colnames(exh_data)<-c(colnames(weather_input)[c8[,c8max]],"Mean_temperature")

exh_trn<-exh_data[trn_idx,]
exh_vld<-exh_data[vld_idx,]

exh_mlr<-lm(Mean_temperature ~ ., data = exh_trn)
trn_pre<-predict(exh_mlr, newdata = exh_vld)
colnames(weather_input)[c8[,c8max]]
exh_mat[8,] <- perf_eval_reg(exh_vld$Mean_temperature, trn_pre)
exh_mat
colnames(weather_input)[c8[,c8max]]

#c9
exh_mat[9,]<-perf_eval_reg(weather_vld$Mean_temperature, prdt_full_mlr)
exh_mat


max(c1_adjRsquared_Table)
max(c2_adjRsquared_Table)
max(c3_adjRsquared_Table)
max(c4_adjRsquared_Table)
max(c5_adjRsquared_Table)
max(c6_adjRsquared_Table)
max(c7_adjRsquared_Table)
max(c8_adjRsquared_Table)
