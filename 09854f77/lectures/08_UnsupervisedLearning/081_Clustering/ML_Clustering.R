## libraries
library(liver)
library(cluster)
library(factoextra)
library(NbClust)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## ######################################
## Aglomerative clustering
## Dendrogram
bank_dist <- daisy(df, metric = "gower")
bank_hc <- hclust(bank_dist)
plot(bank_hc, hang=-1) 

## extract 5 clusters
bank_clus <- cutree(bank_hc, k = 5)
bank_clus <- cbind(Cluster=factor(bank_clus), df)

## Vizualize each cluster in boxplot/barplots
ggplot(bank_clus, aes(y = age, x=Cluster)) +
  geom_boxplot()
ggplot(bank_clus, aes(x=Cluster, fill=job)) + 
  geom_bar()
ggplot(bank_clus, aes(x=Cluster, fill=marital)) + 
  geom_bar()
## etc.

## ######################################
## Partitioning
## K-means

## Use house data for the example because kmeans is limited to numerical features
data(house) 
df <- house[,-c(4,5)] 

## Trial with k=5
df_km <- kmeans(df, centers=5)
df_km$cluster

## Vizualize each cluster in boxplot/barplots
df_km <- cbind(Cluster=factor(df_km$cluster), df)
ggplot(df_km, aes(y = house.age, x=Cluster)) +
  geom_boxplot()
ggplot(df_km, aes(y = distance.to.MRT, x=Cluster)) +
  geom_boxplot()
ggplot(df_km, aes(y = stores.number, x=Cluster)) +
  geom_boxplot()
ggplot(df_km, aes(y = unit.price, x=Cluster)) +
  geom_boxplot()

## Discussion:
## - Why the clustering is so influenced by distance.to.MRT? How to adjust this?

## See clusters in their first two principal components space
df_km <- kmeans(df, centers=5)
fviz_cluster(df_km, data = df) 

## Inspect the number of clusters
fviz_nbclust(df, kmeans, method = "wss", k.max = 15) # method = Within Sum of Squares

## Discussion:
## - Look for an elbow -> what is the optimal nb of clusters? (~3) 

## 
## PAMs 

## Back to bank data because PAM can use gower distance
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"
bank_dist <- daisy(df, metric = "gower")
bank_pam <- pam(bank_dist, k=5)

## Silhouette plot
plot(silhouette(bank_pam), col="darkblue", border=NA) # col and border are needed on Rstudio otherwise nothing will appear

## Vizualization with boxplot/barplots works exactly the same
bank_clus <- cbind(Cluster=factor(bank_pam$clustering), df)
ggplot(bank_clus, aes(y = age, x=Cluster)) +
  geom_boxplot()
ggplot(bank_clus, aes(x=Cluster, fill=job)) + 
  geom_bar()
ggplot(bank_clus, aes(x=Cluster, fill=marital)) + 
  geom_bar()

## ###########################
## Select the number of clusters 
n_clust_min <- 2
n_clust_max <- 20
avg_sil <- numeric(length = n_clust_max - n_clust_min + 1)
i <- 0
for (n_clust in n_clust_min:n_clust_max){
  i <- i+1
  tmp <- pam(bank_dist, k=n_clust)
  tmp2 <- summary(silhouette(tmp))
  avg_sil[i] <- tmp2$avg.width
}
avg_sil <- data.frame(nb_clus=n_clust_min:n_clust_max,
                      silhouette=avg_sil)
plot(silhouette~nb_clus, data=avg_sil, type="b", pch=20)

## ############################
## Using NbClust: a convinient package but only for numerical 
data(house) 
df <- house[,-c(4,5)] 

fviz_nbclust(df, kmeans, method = "silhouette") 
fviz_nbclust(df, pam, method = "silhouette") 
fviz_nbclust(df, pam, method = "wss") 
