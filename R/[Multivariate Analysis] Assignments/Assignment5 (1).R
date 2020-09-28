#Q3

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
perf_mat <- matrix(0, nrow = 3, ncol = 3) 
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
rownames(perf_mat)<-c( "Forward", "Backward", "Stepwise")
perf_mat

weather_trn<-weather_data[trn_idx,]
weather_vld<-weather_data[vld_idx,]

tmp_x <- paste(colnames(weather_trn)[-10], collapse=" + ")
tmp_xy <- paste("Mean_temperature ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

#forward

start_time <- proc.time()
forward_model <- step(lm(Mean_temperature ~ 1, data = weather_trn), 
                      scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1), 
                      direction="forward", trace = 1)
end_time <- proc.time()
end_time - start_time
summary(forward_model)

prdt_forward_mlr <- predict(forward_model, newdata = weather_vld)
perf_mat[1,] <- perf_eval_reg(weather_vld$Mean_temperature, prdt_forward_mlr)
perf_mat


#backward
start_time <- proc.time()
backward_model <- step(lm(Mean_temperature ~ ., data = weather_trn), 
                      scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1), 
                      direction="backward", trace = 1)
end_time <- proc.time()
end_time - start_time
summary(backward_model)

prdt_backward_mlr <- predict(backward_model, newdata = weather_vld)
perf_mat[2,] <- perf_eval_reg(weather_vld$Mean_temperature, prdt_backward_mlr)
perf_mat


#stepwise
start_time <- proc.time()
stepwise_model <- step(lm(Mean_temperature ~ 1, data = weather_trn), 
                       scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1), 
                       direction="both", trace = 1)
end_time <- proc.time()
end_time - start_time
summary(stepwise_model)

prdt_stepwise_mlr <- predict(stepwise_model, newdata = weather_vld)
perf_mat[3,] <- perf_eval_reg(weather_vld$Mean_temperature, prdt_stepwise_mlr)
perf_mat


#Q4
# Variable selection method 4: Genetic Algorithm
# Fitness function: adj_Rsqured for the training dataset

fit_r2 <- function(string){
  sel_var_idx <- which(string == 1)
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- lm(y ~ ., data = xy)
  GA_pre_trn<-predict(GA_lr, newdata = xy)
  
  #return(summary(GA_lr)$adj.r.squared)

  rss <- sum((GA_pre_trn - y) ^ 2)  ## residual sum of squares
  tss <- sum((y - mean(y)) ^ 2)  ## total sum of squares
  adjrsq <- 1-(rss/(length(y)-length(sel_var_idx)-1))/(tss/(length(y)-1))
  return(adjrsq)
}

y <- weather_trn[,10]
x <- weather_trn[,-10]

# Variable selection by Genetic Algorithm
start_time <- proc.time()
GA_r2 <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
            names = colnames(x), popSize = 100, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed=333)
end_time <- proc.time()
end_time - start_time

best_var_idx <- which(GA_r2@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(best_var_idx, 10)]
GA_tst_data <- weather_vld[,c(best_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff
GA_pre_vld<-predict(GA_model, newdata = GA_tst_data)
GA_perf <- matrix(0, nrow = 1, ncol = 3) 
colnames(GA_perf) <- c("RMSE", "MAE", "MAPE")
GA_perf<-perf_eval_reg(GA_tst_data$Mean_temperature,GA_pre_vld)
GA_perf


#Q5
#original
GA_r2 <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
            names = colnames(x), popSize = 100, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed=333)

#1_popsize,iteration
GA_pop <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
            names = colnames(x), popSize = 30, 
            pmutation = 0.01, maxiter = 100, elitism = 2)
posize_var_idx <- which(GA_pop@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(posize_var_idx, 10)]
GA_tst_data <- weather_vld[,c(posize_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff
GA_pre_vld<-predict(GA_model, newdata = GA_tst_data)
GA_perf <- matrix(0, nrow = 1, ncol = 3) 
colnames(GA_perf) <- c("RMSE", "MAE", "MAPE")
GA_perf<-perf_eval_reg(GA_tst_data$Mean_temperature,GA_pre_vld)
GA_perf

#2_popsize,pmutation
GA_pmu <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
             names = colnames(x), popSize = 20, 
             pmutation = 0.1, maxiter = 10, elitism = 1)

pmu_var_idx <- which(GA_pmu@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(pmu_var_idx, 10)]
GA_tst_data <- weather_vld[,c(pmu_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff
GA_pre_vld<-predict(GA_model, newdata = GA_tst_data)
GA_perf <- matrix(0, nrow = 1, ncol = 3) 
colnames(GA_perf) <- c("RMSE", "MAE", "MAPE")
GA_perf<-perf_eval_reg(GA_tst_data$Mean_temperature,GA_pre_vld)
GA_perf

#3_popsize,crosseover_rate
GA_cr <- ga(type = "binary", fitness = fit_r2, nBits = length(x), 
             names = colnames(x), popSize = 15, pcrossover = 0.95,
             pmutation = 0.01, maxiter = 50, elitism = 2)
cr_var_idx <- which(GA_cr@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(cr_var_idx, 10)]
GA_tst_data <- weather_vld[,c(cr_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff
GA_pre_vld<-predict(GA_model, newdata = GA_tst_data)
GA_perf <- matrix(0, nrow = 1, ncol = 3) 
colnames(GA_perf) <- c("RMSE", "MAE", "MAPE")
GA_perf<-perf_eval_reg(GA_tst_data$Mean_temperature,GA_pre_vld)
GA_perf




#4_iteration,pcrossover
GA_iter <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
             names = colnames(x), popSize = 50, pcrossover = 0.95, 
             pmutation = 0.01, maxiter = 15, elitism = 2)

iter_var_idx <- which(GA_iter@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(iter_var_idx, 10)]
GA_tst_data <- weather_vld[,c(iter_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff
GA_pre_vld<-predict(GA_model, newdata = GA_tst_data)
GA_perf <- matrix(0, nrow = 1, ncol = 3) 
colnames(GA_perf) <- c("RMSE", "MAE", "MAPE")
GA_perf<-perf_eval_reg(GA_tst_data$Mean_temperature,GA_pre_vld)
GA_perf
