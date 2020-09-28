#lambda를 1.5->lambda[,819]로 고정시키고 training data를 바꿔가면서 그때의 beta 값을 dataframe으로 모은 뒤
#그것을 박스 플롯으로 그려보자

library(MASS)
attach(Boston)
library(plotmo)

#1) 변수설정
grid <- 10^seq(2,-2,length=500)
nrow(Boston)
ncol(Boston)
Boston <- na.omit(Boston) 
x <- model.matrix(medv~.,Boston)
str(x)
y <-Boston$medv
str(y)

betadata<-c()

for(i in 1:100){
  set.seed(i)
  
  train <- sample(1:nrow(x),round(nrow(x)*.7))
  test <- (-train)
  y.test <- y[test]
  lasso.mod <-glmnet(x[train,], y[train],alpha=1,lambda=grid)
  
  betadata<-rbind(betadata,coef(lasso.mod)[,416])
  
  
}

lasso.mod$lambda[416]

betadata<-data.frame(betadata)

boxplot(betadata, main="Boxplot of Lasso Parameters",horizontal=F,col="plum1")

#예시
set.seed(333)

train <- sample(1:nrow(x),round(nrow(x)*.7))
test <- (-train)
y.test <- y[test]
lasso.mod <-glmnet(x[train,], y[train],alpha=1,lambda=grid)
coef(lasso.mod)[,416]

plot_glmnet(lasso.mod, xvar = "rlambda")
