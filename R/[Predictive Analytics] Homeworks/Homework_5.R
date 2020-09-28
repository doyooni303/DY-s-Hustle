install.packages("class")
install.packages("caret")
install.packages("e1071")
install.packages("‘factoextra")
install.packages("SCORPIUS")
install.packages("KODAMA")
install.packages("amap")
install.packages("labelrank")
install.packages("knnGarden")
install.packages("kknn")
install.packages("ElemStatLearn")
library(ElemStatLearn)
library(kknn)
library(knnGarden)
library(labelrank)
library(amap)
library(KODAMA)
library(class)
library(dplyr)
library(caret)
library(e1071)
library(factoextra)
data("iris")
iris
wine<-read.csv("wine.csv")

wine<-wine[1:178,1:14]
names(wine)[1] <- c("type")
str(wine)
input_idx <- c(2:14)
target_idx <-1

wine_input <- wine[,input_idx]
wine_input <- scale(wine_input, center = TRUE, scale = TRUE)
wine_target <- wine[,target_idx]
wine_data <- data.frame(wine_input,wine_target)

trn_idx <- sample(1:nrow(wine_data), 133)
wine_trn <- wine_data[trn_idx,]
wine_tst <- wine_data[-trn_idx,]

train_x <- wine_trn[, -14]
test_x <- wine_tst[, -14]
train_y<-wine_trn[,14]
test_y<-wine_tst[,14]



missclasserror_k <- NULL
missclass_m<-NULL
# kk가 1부터 train 행 수까지 증가할 때 (반복문)
for(kk in c(1:nrow(train_x))) {
  
  set.seed(1234)
  eu_knn <- man_knn<-kknn(wine_target ~., wine_trn, wine_tst, na.action = na.omit(), 
                         k = kk, distance = 2, kernel = "rectangular", ykernel = NULL, scale=F)
  eu_fit<-fitted(eu_knn)
  # 분류 정확도 계산하기
  missclasserror_k <- c(missclasserror_k, sum(eu_fit != test_y) / length(test_y))
}
missclasserror_k
# k에 따른 분류 정확도 데이터 생성
valid_k <- data.frame(k = c(1:nrow(train_x)), missclassification_error = missclasserror_k)

# k에 따른 분류 정확도 그래프 그리기
plot(formula = missclassification_error ~ k,
     data = valid_k,
     type = "o",
     pch = 20,
     main = "Classification result")

# 그래프에 k 라벨링 하기
with(valid_k, text(valid_k$accuracy ~ k, labels = rownames(valid_k), pos = 1, cex = 0.7))

# 분류 정확도가 가장 높으면서 가장 작은 k는?
min(valid_k[valid_k$accuracy %in% max(accuracy_k), "k"])

set.seed(1234)
knn.80 <-  knn(train=train_x, test=test_x, cl=train_y, k=80)
table1<-table(knn.80 ,test_y)
table1


confusionMatrix(test_y,knn.80)


#Distance measure

set.seed(1234)
man_dist<-get_dist(wine_input, method="manhattan")
summary(man_dist)


#"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"

test_man_dist<-dist(test_x, method = "manhattan", diag = FALSE, upper = T)
man_dist

man_knn<-kNN(man_dist,k=10,search = "dist", splitRule = "suggest", approx = 0)
man_knn<-kNN(man_dist,10)
c<-table(test_y,man_knn)


##distance 바꾸기############################
man_knn<-kknn(wine_target ~., wine_trn, wine_tst, na.action = na.omit(), 
     k = 10, distance = 1, kernel = "rectangular", ykernel = NULL, scale=F)

missclass_m<-NULL
for(kk in c(1:nrow(train_x))){
  
  # k가 kk일 때 knn 적용하기
  set.seed(1234)
  man_knn<-kknn(wine_target ~., wine_trn, wine_tst, na.action = na.omit(), 
                k = kk, distance = 1, kernel = "rectangular", ykernel = NULL, scale=F)
  man_fit<-fitted(man_knn)
  # 분류 정확도 계산하기
  missclass_m <- c(missclass_m, sum(man_fit != test_y) / length(test_y))
}

# k에 따른 분류 정확도 데이터 생성
valid_m <- data.frame(k = c(1:nrow(train_x)), missclassification_error = missclass_m)

# k에 따른 분류 정확도 그래프 그리기
plot(formula = missclassification_error ~ k,
     data = valid_m,
     type = "l",
     pch = 1,
     main = "Classification result")
legend(x = 0, y = 0.7, legend=c("Euclidean", "Manhattan","Mahalanobis"), col=c("red", "blue","darkseagreen"), lty=1:1, bg = "white", 
       text.font=21,cex = 1)

valid_k$k


valid_mh<-read.csv("wine_mahal.csv")
names(valid_mh)[1]<-c("k")
valid_mh<-data.frame(valid_mh$k,valid_mh$missclassification_error)
colnames(valid_mh)<-c("k","missclassification_error")
points(valid_k$k,valid_k$missclassification_error, type = 'l', col = "Red") 
points(valid_m$k,valid_m$missclassification_error, type = 'l', col = "Blue")
points(valid_mh$k,valid_mh$missclassification_error,type = 'l', col= "darkseagreen")
#예측하기
man_fit<-fitted(man_knn)
table(test_y,man_fit)
confusionMatrix(test_y,man_fit)

eu_knn<-kknn(wine_target ~., wine_trn, wine_tst, na.action = na.omit(), 
             k = 10, distance = 2, kernel = "rectangular", ykernel = NULL, scale=F)
eu_fit<-fitted(eu_knn)
table(test_y,eu_fit)
confusionMatrix(test_y,eu_fit)
기

#Decision Boundary
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
prob
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

summary(wine)
# k=1 DB
set.(333)
knn1 <-  knn(train=train_x, test=test_x, cl=train_y, k=1)
lgrid <- expand.grid(Alchol=seq(-1.95931, 2.252341, by=0.0809), 
                     Malic_Acid=seq(-1.42895, 3.10045, by=0.1),
                     Ash = 0,
                     Ash_Alcalinity=0,
                     Magnesium=0, Total_Phenols=0,Flavanoids=0,
                     Nonflavanoid_Phenols=0,Proanthocyanins=0,
                     Color_Intensity=0,Hue=0,OD280_OD315=0,
                     Proline=0)

knnPredGrid = as.numeric(knn1) # 1 2 3
ac = seq(-1.95931, 2.252341, by=0.0809)
ma = seq(-1.42895, 3.10045, by=0.1)

probs <- matrix(knnPredGrid, length(ac), 
                length(ma))

contour(ac, ma, probs,labels="", xlab="Alchol", ylab="Malic_Acid", main=
          "1-nearest neighbour", axes=FALSE)

gd <- expand.grid(x=ac, y=ma)

points(gd, pch=".", cex=5, col=probs)
box()   

#knn10
set.seed(333)
knn10 <-  knn(train=train_x, test=test_x, cl=train_y, k=10)
lgrid <- expand.grid(Alchol=seq(-1.95931, 2.252341, by=0.0809), 
                     Malic_Acid=seq(-1.42895, 3.10045, by=0.1),
                     Ash = 0,
                     Ash_Alcalinity=0,
                     Magnesium=0, Total_Phenols=0,Flavanoids=0,
                     Nonflavanoid_Phenols=0,Proanthocyanins=0,
                     Color_Intensity=0,Hue=0,OD280_OD315=0,
                     Proline=0)


knnPredGrid = as.numeric(knn10) # 1 2 3
ac = seq(-1.95931, 2.252341, by=0.0809)
ma = seq(-1.42895, 3.10045, by=0.1)
probs <- matrix(knnPredGrid, length(ac), 
                length(ma))

contour(ac, ma, probs,labels="", xlab="Alchol", ylab="Malic_Acid", main=
          "10-nearest neighbour", axes=FALSE)

gd <- expand.grid(x=ac, y=ma)

points(gd, pch=".", cex=5, col=probs)
box()   
summary(probs)
