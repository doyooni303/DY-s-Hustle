library(dplyr)

boston<-read.csv("Boston.csv")
boston_x<-boston[,-1]
boston_x<-boston[,-12]

plot(boston_x)

grid <- 10^seq(2,-2,length=100)
nrow(Boston)
ncol(Boston)
Boston <- na.omit(Boston) 
x <- model.matrix(medv~.,boston)

str(x)
y <-boston$medv
str(y)

#1 ordinary one
set.seed(333)

train <- sample(1:nrow(x),round(nrow(x)*.7))
test <- (-train)
y.test <- y[test]
lasso.mod <-glmnet(x[train,], y[train],alpha=1,lambda=grid)

plot_glmnet(lasso.mod, xvar = "rlambda")



boston<-boston[,-1]
boston_x<-boston[,-12]
boston_y<-boston[,12]

cor(boston_x)

