install.packages("lars")
install.packages("glmnet")
library(glmnet)
library(dplyr)
library(moments)
library(lars)
library(ISLR)
library(ggplot2)
set.seed(333)
x1<-sample(-100.0:100.0,1000,replace = T)
x2<-sample(-100:100,1000,replace=T)
e<-rnorm(1000)
x<-cbind(x1,x2)
y<-c()
for(i in 1:nrow(x)) {
  y[i]<-e[i]+6.5*x[i,1]-8.2*x[i,2]
}


data<-data.frame(x1,x2,y)

#1.
#1) 첫번째 training
set.seed(111)
trn_idx<-sample(1:nrow(data),round(0.7*nrow(data)))
trn_idx
training1<-data[trn_idx,]
test1<-data[-trn_idx,]

xtr11<-training1$x1
xtr21<-training1$x2
xtr1<-cbind(xtr11,xtr21)
ytr1<-training1$y

#회귀문석
mlr1 <- lm(y ~ ., data = training1)
summary(mlr1)

#정규화분석
general<-glm(mlr1,data=training1, family=gaussian)
summary(general)

#과연..
cvfit <- glmnet::cv.glmnet(x, y)
coef(cvfit, s = "lambda.1se")
summary(cvfit)
it=glmnet(xtr1,ytr1,alpha = 1)
betadata<-data.frame(cvfit$glmnet.fit$beta$)

summary(fit)
plot(fit)
dev.off()
