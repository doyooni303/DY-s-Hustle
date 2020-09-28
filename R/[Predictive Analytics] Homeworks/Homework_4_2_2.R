library(glmnet)
library(dplyr)
library(moments)
library(lars)
library(ISLR)
library(ggplot2)

#1
set.seed(1234)
x1<-sample(-100.0:100.0,1000,replace = T)
x2<-sample(-1000:1000,1000,replace=T)
x2<-x2/37
x3<-sample(-500:10000,1000,replace=T)
x3<-x3/3000
x4<-sample(-11:9193,1000,replace=T)
x4<-x4/400
x5<-sample(-1000:1000,1000,replace = T)
x5<-x5/1000
e<-rnorm(1000)
x_matrix<-cbind(x1,x2,x3,x4,x5)
x<-data.frame(cbind(x1,x2,x3,x4,x5))
cor(x)
y<-c()
for(i in 1:nrow(x)) {
  y[i]<-e[i]+3.347+6.5*x[i,1]-8.2*x[i,2]+x[i,3]-2.67*x[i,4]+1.56*x[,5]
}
y_matrix<-y
y<-data.frame(y)
plot(x)

#Lasso
grid <- 10^seq(5,-5,length=500)

set.seed(333)

train <- sample(1:nrow(x),round(nrow(x)*.7))
test <- (-train)
y.test <- y_matrix[test]
lasso <-glmnet(x_matrix[train,], y_matrix[train],alpha=1,lambda=grid)
coef(lasso.mod)[,416]

plot_glmnet(lasso, xvar = "rlambda")


#2
set.seed(333)
a1<-sample(-100.0:100.0,1000,replace = T)
a1<-a1/300
a2<-sample(-1000:1000,1000,replace=T)
a2<-x2/37
a3<-sample(-500:10000,1000,replace=T)
a3<-x3/3000
a4<-sample(-11:9193,1000,replace=T)
a4<-x4/400
a5<-4.72*a1-3.27*a2--5.07*a3+10.13*a4+e
e<-rnorm(1000)
a_matrix<-cbind(a1,a2,a3,a4,a5)
a<-data.frame(a_matrix)
cor(a)
plot(a)
b<-c()
for(i in 1:nrow(x)) {
  b[i]<-e[i]+3.347+6.5*a[i,1]-8.2*a[i,2]+a[i,3]-2.67*a[i,4]+1.56*a[,5]
}
b_matrix<-b
b<-data.frame(b)

#Lasso
grid <- 10^seq(5,-5,length=500)

set.seed(333)

train <- sample(1:nrow(x),round(nrow(x)*.7))
test <- (-train)
b.test <- b_matrix[test]
lasso <-glmnet(a_matrix[train,], b_matrix[train],alpha=1,lambda=grid)
coef(lasso.mod)[,416]

plot_glmnet(lasso, xvar = "rlambda")
