#data<-read.csv(file = 'C:/workspace/OpenFoodFactsProject/en.openfoodfacts.org.products.tsv', sep = '\t', header = TRUE)
library(dplyr)
library(reshape2)
library(cluster)
library(HSAUR)
library(ade4)
library("fpc")
library("factoextra")
library (cluster)
library (vegan)
library(fpc)

#rm(list=ls())

# get a sample of data 
datacopy <- data[1:10000,];

#only pick numeric features
nums <- sapply(datacopy, is.numeric)
datacopy <- datacopy[ , nums]

#out of the all records if a features has more than 80% NA remove that feature from the list
datacopy <- datacopy[,colSums(is.na(datacopy))<0.8*colSums(!is.na(datacopy))]

oldDataCopy = datacopy;

# remove the NA out of the remaing features
datacopy <- na.omit(datacopy)

#summary(datacopy)


# normalize function 

#dataScaled = datacopy[,-1];

 dataScaled <- scale(datacopy[,-1])
# 
# 
for(i in 1:15) {
  dataScaled <- dataScaled[dataScaled[,i] <=1,]
}



summary(dataScaled)

# Determine number of clusters
 wss <- (nrow(dataScaled)-1)*sum(apply(dataScaled,2,var))
 for (i in 2:50) wss[i] <- sum(kmeans(dataScaled, centers=i)$withinss)
 plot(1:50, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
 


# Elbow method
 fviz_nbclust(dataScaled, kmeans, method = "wss") + geom_vline(xintercept = 14, linetype = 2)+labs(subtitle = "Elbow method")

# Silhouette method
#fviz_nbclust(dataScaled, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")


# K-Means Cluster Analysis
fit <- kmeans(dataScaled, 10) # 6 cluster solution

# get cluster means 
# aggregate(dataScaled,by=list(fit$cluster),FUN=mean)

# append cluster assignment
# dataScaled <- data.frame(dataScaled, fit$cluster)


#kmeansRes<-factor(fit$cluster)
#plot(dataScaled)
#s.class(dataScaled,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))

# vary parameters for most readable graph
#library(cluster) 
#clusplot(dataScaled, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


# Centroid Plot against 1st 2 discriminant functions
#plotcluster(dataScaled, fit$cluster)


# dissE <- daisy(dataScaled) 
# dE2   <- dissE^2
# sk2   <- silhouette(fit$cluster, dE2)
# plot(sk2)

dis = vegdist(dataScaled)
sil = silhouette (fit$cluster,dis) # or use your cluster vector
windows() # RStudio sometimes does not display silhouette plots correctly
plot(sil)


fviz_cluster(fit, data = dataScaled)