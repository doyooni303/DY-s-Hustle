college_scaled<-scale(college,center=T,scale=T)

#Q2_1_Hierarchical_clustering
cor_Mat <- cor(t(college_scaled), method = "spearman")
college_scaled_clValid <- clValid(college_scaled, 2:10, clMethods = "hierarchical", 
                           validation = c("internal", "stability"))
summary(college_scaled_clValid)


#Q2_2
cor_Mat <- cor(t(college_scaled), method = "spearman")
dist_college <- as.dist(1-cor_Mat)

#single linkage
hierarchical1<-hclust(dist_college, method="single",members=NULL)
plot(hierarchical1)
plot(hierarchical1,hang=-1)
plot(as.dendrogram(hierarchical1),edgePar=list(col=1, lwd=0.5),main="single linkage")

#colmplete linkage
hierarchical2<-hclust(dist_college, method="complete",members=NULL)
plot(hierarchical2)
plot(hierarchical2,hang=-1)
plot(as.dendrogram(hierarchical2),edgePar=list(col=2, lwd=0.5),main="complete linkage")

#average linkage
hierarchical3<-hclust(dist_college, method="average",members=NULL)
plot(hierarchical3)
plot(hierarchical3,hang=-1)
plot(as.dendrogram(hierarchical3),edgePar=list(col=3, lwd=0.5),main="average linkage")

#centroid linkage
hierarchical4<-hclust(dist_college, method="centroid",members=NULL)
plot(hierarchical4)
plot(hierarchical4,hang=-1)
plot(as.dendrogram(hierarchical4),edgePar=list(col=4, lwd=0.5),main="centroid linkage")



#Q2_3
mycl <- cutree(hierarchcial2, k=10)
mycl

plot(as.dendrogram(hierarchical2),edgePar=list(col=2, lwd=0.5),main="complete linkage")
rect.hclust(hierarchical2, k=10, border="deepskyblue")


# Compare each cluster for HC
College$Private<-as.numeric(College$Private)
college_hc <- data.frame(college_scaled, collegeYN = College[,1], 
                       clusterID = as.factor(mycl))
hc_summary2 <- data.frame()

for (i in 1:(ncol(college_hc)-1)){
  hc_summary2 = rbind(hc_summary2, 
                     tapply(college_hc[,i], college_hc$clusterID, mean))
}

colnames(hc_summary2) <- paste("cluster", c(1:10))
rownames(hc_summary2) <- c(colnames(college_scaled),"Private ratio")
hc_summary2

# Radar chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary2[,i],labels = rownames(hc_summary2), 
              radial.lim=c(-2.5,2.5), rp.type = "p", main = plot_title, 
              line.col = "blue3", lwd = 1, show.grid.labels=2)
}
dev.off()


#Similar_Campare the cluster 6&7
hc_cluster6 <- college_hc[college_hc$clusterID == 6, c(1:17)]
hc_cluster7 <- college_hc[college_hc$clusterID == 7, c(1:17)]


hc_t_result_s <- data.frame()

for (i in 1:17){
  
  hc_t_result_s[i,1] <- t.test(hc_cluster6[,i], hc_cluster7[,i], 
                               alternative = "two.sided")$p.value
  
  hc_t_result_s[i,2] <- t.test(hc_cluster6[,i], hc_cluster7[,i], 
                               alternative = "greater")$p.value
  
  hc_t_result_s[i,3] <- t.test(hc_cluster6[,i], hc_cluster7[,i], 
                               alternative = "less")$p.value
}

hc_t_result_s

# Dfferene_Compare the cluster 7 & 8
hc_cluster7 <- college_hc[college_hc$clusterID == 7, c(1:17)]
hc_cluster8 <- college_hc[college_hc$clusterID == 8, c(1:17)]


hc_t_result_d <- data.frame()

for (i in 1:17){
  
  hc_t_result_d[i,1] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                             alternative = "two.sided")$p.value
  
  hc_t_result_d[i,2] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                             alternative = "greater")$p.value
  
  hc_t_result_d[i,3] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                             alternative = "less")$p.value
}

hc_t_result_d

