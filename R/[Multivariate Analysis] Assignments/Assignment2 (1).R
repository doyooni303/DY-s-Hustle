install.packages("ISLR")
install.packages("clValid")
library(ISLR)
library(ggplot2)
data("College")
View(College)

library(clValid)
library(plotrix)

#Data scaling
college<-College[,-1]
norm_college <- scale(college, center = TRUE, scale = TRUE)

#Q1_1 : 군집 수에 따른 internal,stability 지표 산출
college_clValid <- clValid(norm_college, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"))
summary(college_clValid)

#Q1_2
#1
college_kmc1 <- kmeans(norm_college,3)
college_kmc1$centers
college_kmc1$size



#2
college_kmc2 <- kmeans(norm_college,3)
college_kmc2$centers
college_kmc2$size


#3
college_kmc3 <- kmeans(norm_college,3)
college_kmc3$centers
college_kmc3$size

#4
college_kmc4<- kmeans(norm_college,3)
college_kmc4$centers
college_kmc4$size

#5
college_kmc5 <- kmeans(norm_college,3)
college_kmc5$centers
college_kmc5$size

#6
college_kmc6 <- kmeans(norm_college,3)
college_kmc6$centers
college_kmc6$size

#7
college_kmc7 <- kmeans(norm_college,3)
college_kmc7$centers
college_kmc7$size

#8
college_kmc8 <- kmeans(norm_college,3)
college_kmc8$centers
college_kmc8$size

#9
college_kmc9 <- kmeans(norm_college,3)
college_kmc9$centers
college_kmc9$size

#10
college_kmc10 <- kmeans(norm_college,3)
college_kmc10$centers
college_kmc10$size



#Q1_3
#1
college_kmc11 <- kmeans(norm_college,10)
college_kmc11$centers
college_kmc11$size

#2
college_kmc21 <- kmeans(norm_college,10)
college_kmc21$centers
college_kmc21$size

#3
college_kmc31 <- kmeans(norm_college,10)
college_kmc31$centers
college_kmc31$size

#4
college_kmc41 <- kmeans(norm_college,10)
college_kmc41$centers
college_kmc41$size

#5
college_kmc51 <- kmeans(norm_college,10)
college_kmc51$centers
college_kmc51$size

#6
college_kmc61 <- kmeans(norm_college,10)
college_kmc61$centers
college_kmc61$size

#7
college_kmc71 <- kmeans(norm_college,10)
college_kmc71$centers
college_kmc71$size

#8
college_kmc81 <- kmeans(norm_college,10)
college_kmc81$centers
college_kmc81$size

#9
college_kmc91 <- kmeans(norm_college,10)
college_kmc91$centers
college_kmc91$size

#10
college_kmc101 <- kmeans(norm_college,10)
college_kmc101$centers
college_kmc101$size


clustersize<-data.frame(college_kmc11$size,college_kmc21$size,college_kmc31$size,college_kmc41$size,college_kmc51$size,
                        college_kmc61$size,college_kmc71$size,college_kmc81$size,college_kmc91$size,college_kmc101$size)
colnames(clustersize) <- c("1","2","3","4","5","6","7","8","9","10")

clustersize


#Q1_4
college_kmc_rc<-kmeans(norm_college,3)

cluster_kmc <- data.frame(norm_college, clusterID = as.factor(college_kmc_rc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(college)
kmc_summary


par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2.5,2.5), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 2, show.grid.labels=1)
}
dev.off()


#Q1_5
#1) Compare cluster1&cluster2
kmc_cluster1 <- college[college_kmc_rc$cluster == 1,]
kmc_cluster2 <- college[college_kmc_rc$cluster == 2,]

# t_test_result
kmc_t_result1 <- data.frame()

for (i in 1:17){
  
  kmc_t_result1[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result1[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result1[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}
# Compare cluster1&cluster2
kmc_t_result1


#2) Compare cluster2&cluster3
kmc_cluster3 <- college[college_kmc_rc$cluster == 3,]
kmc_cluster2 <- college[college_kmc_rc$cluster == 2,]

# t_test_result
kmc_t_result2 <- data.frame()

for (i in 1:17){
  
  kmc_t_result2[i,1] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                               alternative = "two.sided")$p.value
  
  kmc_t_result2[i,2] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                               alternative = "greater")$p.value
  
  kmc_t_result2[i,3] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                               alternative = "less")$p.value
}
# Compare cluster2&cluster3
kmc_t_result2

6.279872e-01

#3) Compare cluster3&cluster1
kmc_t_result3 <- data.frame()

for (i in 1:17){
  
  kmc_t_result3[i,1] <- t.test(kmc_cluster3[,i], kmc_cluster1[,i], 
                               alternative = "two.sided")$p.value
  
  kmc_t_result3[i,2] <- t.test(kmc_cluster3[,i], kmc_cluster1[,i], 
                               alternative = "greater")$p.value
  
  kmc_t_result3[i,3] <- t.test(kmc_cluster3[,i], kmc_cluster1[,i], 
                               alternative = "less")$p.value
}
# Compare cluster3&cluster1
kmc_t_result3


#Q1_6_visulaization
#plot(college_kmc_rc$centers[,1:2],col=1:3,pch=2,cex=1)


q16kmc<-data.frame(cluster_kmc$F.Undergrad,cluster_kmc$Outstate,cluster_kmc$clusterID)
colnames(q16kmc) <- c("F.Undergrad", "Outstate", "ClusterID")
#plot(x = q16kmc$F.Undergrad,
#    y = q16kmc$Outstate,ylab = "Outstate", xlab = "F.Undergrad",main = "K-means clustering Result",
     #xlim = c(-1, 1),
     #ylim = c(-3, 3),
     #col = c("red", "blue", "green")[q16kmc$clusterID])

g<-ggplot(data = q16kmc,
       mapping = aes(x = Outstate,
                     y = F.Undergrad)) + geom_point(colour = c("red", "blue", "black")[q16kmc$ClusterID],
                                                    pch = c(0, 2, 20)[q16kmc$ClusterID],
                            size = c(1, 1.5, 2)[q16kmc$ClusterID])
g + labs(title = "K-means Clustering(Outstate,F.Undergrad)",
         subtitle = "Cluster1-red, Cluster2-blue, Cluster3-black")
dev.off()
