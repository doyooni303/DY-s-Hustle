#column 제외, 표준정규화
library(plotrix)
library(dplyr)
loan<-read.csv("Personal Loan.csv")
loan_x<-loan[,-c(1,5,10)]
loan_x_scaled<-scale(loan_x,center = T,scale = T)
df_loan<-subset(loan_x_scaled, select=c(Income,Education))


#Q3_1
library(factoextra)
library(dbscan)
library(plotrix)
library(fpc)
dev.off()


DBSCAN_multishapes <- dbscan(loan_x_scaled, eps = 5, MinPts = 30)
t<-table(DBSCAN_multishapes$cluster,loan$Personal.Loan)
t
sum(t[1,])

fviz_cluster(DBSCAN_multishapes, loan_x_scaled, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)
plot.new()


#Q3_2
#(2,10)
set.seed(12345)
DBSCAN_multishapes0 <- dbscan(loan_x_scaled, eps = 2, MinPts = 10)
loan_db<-data.frame(loan_x_scaled, loan$Personal.Loan, clusterID=as.factor(DBSCAN_multishapes0$cluster))
db_summary0<-data.frame()

for (i in 1:(ncol(loan_db)-1)){
  db_summary0 = rbind(db_summary0, 
                      tapply(loan_db[,i], loan_db$clusterID, mean))
}

loan_db$clusterID

colnames(db_summary0) <- paste("cluster", c(0:7))
rownames(db_summary0) <- c(colnames(loan_x_scaled),"Loan ratio")
db_summary0


table(loan_db$clusterID,loan$Personal.Loan)

par(mfrow = c(2,4))
for (i in 1:8){
  plot_title <- paste("Radar Chart for Cluster", i-1, sep=" ")
  radial.plot(db_summary0[,i],labels = rownames(db_summary0), 
              radial.lim=c(-2.5,2.5), rp.type = "p", main = plot_title, 
              line.col = "blue3", lwd = 1, show.grid.labels=2)
}
dev.off()

nrow(loan_db%>%filter(clusterID==0))

#(2,25)
DBSCAN_multishapes1 <- dbscan(loan_x_scaled, eps = 2, MinPts = 25)
loan_db1<-data.frame(loan_x_scaled, loan$Personal.Loan, clusterID=as.factor(DBSCAN_multishapes1$cluster))
db_summary1<-data.frame()

for (i in 1:(ncol(loan_db1)-1)){
  db_summary1 = rbind(db_summary1, 
                      tapply(loan_db1[,i], loan_db1$clusterID, mean))
}

colnames(db_summary1) <- paste("cluster", c(0:5))
rownames(db_summary1) <- c(colnames(loan_x_scaled),"Loan ratio")
db_summary1


table(loan_db1$clusterID,loan$Personal.Loan)

par(mfrow = c(2,3))
for (i in 1:6){
  plot_title <- paste("Radar Chart for Cluster", i-1, sep=" ")
  radial.plot(db_summary1[,i],labels = rownames(db_summary1), 
              radial.lim=c(-2.5,2.5), rp.type = "p", main = plot_title, 
              line.col = "blue3", lwd = 1, show.grid.labels=2)
}
dev.off()
nrow(loan_db1%>%filter(clusterID==0))

#(3,15)
DBSCAN_multishapes2 <- dbscan(loan_x_scaled, eps = 3, MinPts = 15)
loan_db2<-data.frame(loan_x_scaled, loan$Personal.Loan, clusterID=as.factor(DBSCAN_multishapes2$cluster))
db_summary2<-data.frame()

for (i in 1:(ncol(loan_db2)-1)){
  db_summary2 = rbind(db_summary2, 
                      tapply(loan_db2[,i], loan_db2$clusterID, mean))
}

colnames(db_summary2) <- paste("cluster", c(0:4))
rownames(db_summary2) <- c(colnames(loan_x_scaled),"Loan ratio")
db_summary2


table(loan_db2$clusterID,loan$Personal.Loan)

par(mfrow = c(2,3))
for (i in 1:5){
  plot_title <- paste("Radar Chart for Cluster", i-1, sep=" ")
  radial.plot(db_summary2[,i],labels = rownames(db_summary2), 
              radial.lim=c(-2.5,2.5), rp.type = "p", main = plot_title, 
              line.col = "blue3", lwd = 1, show.grid.labels=2)
}
dev.off()
nrow(loan_db2%>%filter(clusterID==0))

#Q3_3
install.packages("devtools")
install.packages("ggbiplot")
install.packages("ggfortify")
library(devtools)
library(ggplot2)
library(ggfortify)

loan315<-loan_db2[,-13]
cluster315<-loan_db2[,13]

pca_loan315 <- prcomp(loan315,scale=T)
pca_loan315
plot(pca_loan315,type = "l")
summary(pca_loan315)

autoplot(pca_loan315, data = loan_db2, colour = 'clusterID')

autoplot(pca_loan, data = loan_db, colour = 'clusterID')
summary(pca_loan)




