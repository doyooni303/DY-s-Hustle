setwd("G:/내 드라이브/학교/4학년 1학기/예측애널리틱스/PA.project")
library(tree)
library(randomForest)
library(glmnet)
set.seed(931113)

# 데이터 로드

df = read.csv("spotify_classification.csv")
head(df)
cat('# of na value :',sum(is.na(df)),', dim of dataset :',dim(df))
str(df)
summary(df)
boxplot(spotify_data)
# 단변수 데이터 시각화
for (col in colnames(df)){
    if (col %in% c('X','song_title','artist')){next}
    hist(df[,col],main=col)
}

# 데이터 Split

idx = sample(c('trn','tst','val'),nrow(df),replace=TRUE,prob=c(0.6,0.2,0.2))

df.trn = subset(df,select= -c(X,song_title,artist))[idx=='trn',]
df.val = subset(df,select= -c(X,song_title,artist))[idx=='val',]
df.tst = subset(df,select= -c(X,song_title,artist))[idx=='tst',]
df.trn_d=df.trn[,-4]
df.val_d=df.val[,-4]
df.tst_d=df.tst[,-4]

perf_eval = function(cm){
    
    # True positive rate: TPR (Recall)
    TPR <- cm[2,2]/sum(cm[2,])
    # Precision
    PRE <- cm[2,2]/sum(cm[,2])
    # Simple Accuracy
    ACC <- (cm[1,1]+cm[2,2])/sum(cm)
    # F1-Measure
    F1 <- 2*TPR*PRE/(TPR+PRE)
    
    return(c(TPR, PRE, ACC, F1))
}

Perf.Table = matrix(0, nrow = 12, ncol = 5)
rownames(Perf.Table) = c('Logistic Regression','Forward Selection','Backward Elimination','Stepwise Selection',
                         'Ridge','LASSO','Elastic Net','Decision Tree(Deviance)','Decision Tree(Gini)',
                         'Random Forest','ANN','ANN (5-fold)')
colnames(Perf.Table) = c("Recall", "Precision", "Accuracy","F1-Measure",'AUC')
cutoff = 0.5 # cutoff에 따른 recall, precision trade-off

## 모델 구축 1 : Logistic Regression
vif(LR.trn)
# full-model
LR.trn = glm(target ~ ., family=binomial, df.trn)
summary(LR.trn)
logistic=glm(target~ acousticness+ danceability+ duration_ms+ instrumntalness+ loudness+ speechiness+ valence, family=binomial, df.trn)
summary(logistic)
vif(logistic)

response = predict(LR.trn,type="response",df.val)
yhat=rep(0, nrow(df.val))
yhat[which(response >= cutoff)] = 1
table=table(df.val$target,yhat)
table
Perf.Table[1,1:4] = perf_eval(table)
Perf.Table[1,5] = auc(df.val_d$target,response)
Perf.Table


LR.response = predict(LR.trn, type = "response", df.val)
LR.yhat = rep(0, nrow(df.val))
LR.yhat[which(LR.response >= cutoff)] = 1
cm.table = table(df.val$target, LR.yhat)
cm.table
Perf.Table[1,1:4] = perf_eval(cm.table)
Perf.Table[1,5] = auc(df.val$target,LR.response)
Perf.Table

# variable selection 1-1 : Forward selection

tmp_x = paste(colnames(df.trn)[-14], collapse=" + ")
tmp_xy = paste("target ~ ", tmp_x, collapse = "")

FS.trn = step(glm(target ~ 1,data=df.trn), scope = list(upper = as.formula(tmp_xy), lower = target ~ 1), 
                      direction="forward", trace = 1)
summary(FS.trn)

FS.response = predict(FS.trn, type = "response", df.val)
FS.yhat = rep(0,nrow(df.val))
FS.yhat[which(FS.response >= cutoff)] = 1
cm.table = table(df.val$target, FS.yhat)
cm.table

Perf.Table[2,1:4] = perf_eval(cm.table)
Perf.Table[2,5] = auc(df.val$target,FS.response)
Perf.Table

# variable selection 1-2 : Backward Elimination

BE.trn = step(LR.trn, scope = list(upper = as.formula(tmp_xy), lower = target ~ 1), 
              direction="backward", trace = 1)
summary(BE.trn)

BE.response = predict(BE.trn, type = "response", df.val)
BE.yhat = rep(0,nrow(df.val))
BE.yhat[which(BE.response >= cutoff)] = 1
cm.table = table(df.val$target, BE.yhat)
cm.table

Perf.Table[3,1:4] = perf_eval(cm.table)
Perf.Table[3,5] = auc(df.val$target,BE.response)
Perf.Table

# variable selection 1-3 : Stepwise Selection

SS.trn = step(LR.trn, scope = list(upper = as.formula(tmp_xy), lower = target ~ 1), 
              direction="both", trace = 1)
summary(SS.trn)

SS.response = predict(SS.trn, type = "response", df.val)
SS.yhat = rep(0,nrow(df.val))
SS.yhat[which(SS.response >= cutoff)] = 1
cm.table = table(df.val$target, SS.yhat)
cm.table

Perf.Table[4,1:4] = perf_eval(cm.table)
Perf.Table[4,5] = auc(df.val$target,SS.response)
Perf.Table



### 모델 구축 2 : Shrinkage

SM.trn.x = as.matrix(df.trn[,-14])
SM.trn.y = df.trn[,14]
SM.val.x = as.matrix(df.val[,-14])
SM.val.y = df.val[,14]

Shrkg.Table = matrix(0, nrow = 2, ncol = 2)
# shrinkage method 2-1 : Ridge logistic regression

RM.trn = glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 0)
plot(RM.trn, xvar = "lambda",main="Ridge Model")

CV.Ridge = cv.glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 0, nfolds = 5)
plot(CV.Ridge)
best.lambda = CV.Ridge$lambda.min
best.lambda
RM.coeff = predict(RM.trn, s = best.lambda, newx = SM.val.x, type = "coefficient")
RM.coeff
RM.yhat = predict(RM.trn, s = best.lambda, newx = SM.val.x, type = "class")
RM.yhat = as.factor(RM.yhat)
RM.cfm = table(SM.val.y, RM.yhat)
RM.cfm
Perf.Table[5,1:4] = perf_eval(RM.cfm)
Perf.Table[5,5] = auc(SM.val.y,RM.yhat)
Perf.Table

# Shrinkage method 2-2 : Lasso regression

LM.trn = glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 1)
plot(LM.trn, xvar = "lambda",main="LASSO Model")

CV.Lasso = cv.glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 1, nfolds = 5)
plot(CV.Lasso)
best.lambda = CV.Lasso$lambda.min
best.lambda
summary(CV.Lasso)
LM.coeff = predict(LM.trn, s = best.lambda, newx = SM.val.x, type = "coefficient")
LM.coeff
LM.yhat = predict(LM.trn, s = best.lambda, newx = SM.val.x, type = "class")
LM.yhat = as.factor(LM.yhat)
LM.cfm = table(SM.val.y, LM.yhat)
LM.cfm
Perf.Table[6,1:4] = perf_eval(LM.cfm)
Perf.Table[6,5] = auc(SM.val.y,LM.yhat)
Perf.Table

# Shrinkage method 2-3 : Elastic net regression

EN.trn = glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 0.5)
plot(EN.trn, xvar = "lambda",main="Elastic Net Model")

CV.Elastic = cv.glmnet(SM.trn.x, SM.trn.y, family = "binomial", alpha = 0.5, nfolds = 5)
plot(CV.Elastic)
best_lambda = CV.Elastic$lambda.min
best_lambda
EN.coeff = predict(EN.trn, s = best_lambda, newx = SM.val.x, type = "coefficient")
EN.coeff
EN.yhat = predict(EN.trn, s = best_lambda, newx = SM.val.x, type = "class")
EN.yhat = as.factor(EN.yhat)
EN.cfm = table(SM.val.y, EN.yhat)
EN.cfm
Perf.Table[7,1:4] = perf_eval(EN.cfm)
Perf.Table[7,5] = auc(SM.val.y,EN.yhat)
Perf.Table



## 모델 구축 3 : Decision Tree

# split creteria 3-1 : deviance

df.trn[,'target'] = as.factor(df.trn[,'target'])
DT.trn = tree(target ~ .,df.trn)
summary(DT.trn)

DT.cv = cv.tree(DT.trn, FUN = prune.misclass)
plot(DT.cv$size, DT.cv$dev, type = "b")
DT.cv

DT.trn.pruned = prune.misclass(DT.trn, best = DT.cv$size[max(which(DT.cv$dev == min(DT.cv$dev)))])
plot(DT.trn.pruned)
text(DT.trn.pruned, pretty = 1)
summary(DT.trn.pruned)

DT.yhat = predict(DT.trn.pruned, df.val, type = "class")
DT.cfm = table(df.val$target, DT.yhat)
DT.cfm

Perf.Table[8,1:4] = perf_eval(DT.cfm)
Perf.Table[8,5] = auc(df.val$target,predict(DT.trn.pruned, df.val, type = "vector")[,2])
Perf.Table

# split creteria 3-2 : gini index

DT.trn = tree(target ~ .,df.trn,split='gini')
summary(DT.trn)

DT.cv = cv.tree(DT.trn, FUN = prune.misclass)
plot(DT.cv$size, DT.cv$dev, type = "b")
DT.cv
DT.cv$size[max(which(DT.cv$dev == min(DT.cv$dev)))]
DT.trn.pruned = prune.misclass(DT.trn, best = DT.cv$size[max(which(DT.cv$dev == min(DT.cv$dev)))])
plot(DT.trn.pruned)
text(DT.trn.pruned, pretty = 1)
summary(DT.trn.pruned)

DT.yhat = predict(DT.trn.pruned, df.val, type = "class")
DT.cfm = table(df.val$target, DT.yhat)
DT.cfm

Perf.Table[9,1:4] = perf_eval(DT.cfm)
Perf.Table[9,5] = auc(df.val$target,predict(DT.trn.pruned, df.val, type = "vector")[,2])
Perf.Table



### 모델 구축 4 : Random Forest

RF.trn = randomForest(target ~ .,df.trn, ntree = 2000, mtry=3)
RF.trn
summary(RF.trn)
plot(RF.trn)
order(importance(RF.trn))
legend(x = 1500, y = 0.35, legend=c("Prefer", "OOB","Not Prefer"), col=c("green", "black","red"), lty=1:1, bg = "white", 
       text.font=21,cex = 1)
RF.yhat = predict(RF.trn, df.val, type = "class")
RF.cfm = table(df.val$target, RF.yhat)
RF.cfm
importance(RF.trn)
Perf.Table[10,1:4] = perf_eval(RF.cfm)
Perf.Table[10,5] = auc(df.val$target,predict(RF.trn, df.val, type = "prob")[,2])
Perf.Table

### 모델 구축 5 : ANN

# ANN method 5-1 : full observation

ann.trn.x = scale(df.trn[,-14],center=TRUE,scale=TRUE)
ann.trn.y = as.integer(as.character(df.trn[,14]))

ann.val.x = scale(df.val[,-14],center=TRUE,scale=TRUE)
ann.val.y = as.integer(as.character(df.val[,14]))

hidden.node = seq(from=5, to=40, by=5)
ANN.matrix = matrix(0,8,6)

for (i in 1:length(hidden.node)){
    
    ANN.trn = nnet(ann.trn.x, ann.trn.y, size = hidden.node[i], maxit = 200)
    
    ANN.yhat = predict(ANN.trn,ann.val.x)
    ANN.yhat[which(ANN.yhat>=cutoff)] = as.integer(1)
    ANN.yhat[which(ANN.yhat<cutoff)] = as.integer(0)
    
    ANN.cfm = table(ann.val.y,ANN.yhat)
    ANN.auc = auc(ann.val.y,as.integer(ANN.yhat))
    ANN.matrix[i,2:5] = perf_eval(ANN.cfm)
    ANN.matrix[i,6] = ANN.auc
}

ANN.matrix[,1] = hidden.node
ANN.matrix = ANN.matrix[order(-ANN.matrix[,2]),]

colnames(ANN.matrix)=c("No.Hidden Nodes",colnames(Perf.Table))
ANN.matrix
Perf.Table[11,] = ANN.matrix[1,2:6]

Perf.Table
# ANN method 5-2 : 5-fold validation, 전체 Data 1/5 사용

sample.idx = sample(1:nrow(df.trn),nrow(df.trn)/5,replace=FALSE)
akf.trn.x = scale(df.trn[sample.idx,-14],center=TRUE,scale=TRUE)
akf.trn.y = as.integer(as.character(df.trn[sample.idx,14]))

hidden.node = seq(from=5, to=40, by=5)
vd.idx = sample(c(1:5), dim(akf.trn.x)[1], replace = TRUE, prob = rep(0.2,5))
vd.perf = matrix(0, length(vd.idx), 3)

AKF.matrix = matrix(0,8,6)

for (i in 1:length(hidden.node)){
    
    eval.fold = c()
    
    for (j in c(1:5)) {
        tmp.trn.x = akf.trn.x[which(vd.idx!= j),]
        tmp.trn.y = akf.trn.y[which(vd.idx!= j)]    
        tmp.nnet = nnet(tmp.trn.x, tmp.trn.y, size = hidden.node[i], maxit = 200)
        
        tmp.val.x = akf.trn.x[which(vd.idx == j),]
        tmp.val.y = akf.trn.y[which(vd.idx == j)]    
        
        tmp.yhat = predict(tmp.nnet,tmp.val.x)
        tmp.yhat[which(tmp.yhat>=cutoff)] = as.integer(1)
        tmp.yhat[which(tmp.yhat<cutoff)] = as.integer(0)
        eval.fold = rbind(eval.fold,cbind(tmp.val.y,tmp.yhat))
    }
    
    ANN.cfm = table(eval.fold[,1], eval.fold[,2])
    AKF.auc = auc(eval.fold[,1],eval.fold[,2])
    AKF.matrix[i,2:5] = perf_eval(ANN.cfm)
    AKF.matrix[i,6] = AKF.auc
}

AKF.matrix[,1] = hidden.node
AKF.matrix = AKF.matrix[order(-AKF.matrix[,2]),]
colnames(AKF.matrix)=c("No.Hidden Nodes",colnames(Perf.Table))
AKF.matrix
Perf.Table[12,] = AKF.matrix[1,2:6]

Perf.Table[order(-Perf.Table[,1]),]

### 모델 선정 : Backward Elimination, Random Forest, ANN (5-fold)

Test.Table = matrix(0, nrow = 3, ncol = 5)
rownames(Test.Table) = c('Backward Elimination','Random Forest','ANN (5-fold)')
colnames(Test.Table) = c("Recall", "Precision", "Accuracy","F1-Measure",'AUC')

# Test 1 : Backward Elimination

BE.response.0 = predict(BE.trn, type = "response", df.tst)
BE.yhat.0 = rep(0,nrow(df.tst))
BE.yhat.0[which(BE.response.0 >= cutoff)] = 1
cm.table = table(df.tst$target, BE.yhat.0)
cm.table

Test.Table[1,1:4] = perf_eval(cm.table)
Test.Table[1,5] = auc(df.tst$target,BE.response.0)
Test.Table

# Test 2 : Random Forest

RF.yhat.0 = predict(RF.trn, df.tst, type = "class")
RF.cfm = table(df.tst$target, RF.yhat.0)
RF.cfm

Test.Table[2,1:4] = perf_eval(RF.cfm)
Test.Table[2,5] = auc(df.tst$target,predict(RF.trn, df.tst, type = "prob")[,2])
Test.Table

# Test 3 : ANN (5-fold)

akf.tst.x = scale(df.tst[,-14],center=TRUE,scale=TRUE)
akf.tst.y = as.integer(as.character(df.tst[,14]))

hidden.node = AKF.matrix[1,1]

AKF.nnet = nnet(tmp.trn.x, tmp.trn.y, size = hidden.node, maxit = 200)

AKF.yhat.0 = predict(AKF.nnet,akf.tst.x)
AKF.yhat.0[which(AKF.yhat.0>=cutoff)] = as.integer(1)
AKF.yhat.0[which(AKF.yhat.0<cutoff)] = as.integer(0)
AKF.cfm = table(akf.tst.y, AKF.yhat.0)
AKF.auc = auc(akf.tst.y, AKF.yhat.0)

Test.Table[3,1:4] = perf_eval(AKF.cfm)
Test.Table[3,5] = AKF.auc

Test.Table


