data <- read.csv("EastWestAirlinesCluster (1).csv")
data <- data[,-1] #removed ID
#### part a ####

#normalising
data.norm <- sapply(data, scale)

# compute normalized distance based on all 8 variables
dist.norm <- dist(data.norm, method = "euclidean")

hc1 <- hclust(dist.norm, method = "ward.D")

#For dendogram
install.packages("dendextend")
library(dendextend)
dend <- as.dendrogram(hc1)

k <- 6 
dend <- color_branches(dend, k = k)
plot(dend, main = "Dendrogram with Colored Clusters")

#### part c ####

k <- 6
# clusters for each observation
cluster_assignments <- cutree(hc1, k = k)

data_with_clusters <- data
data_with_clusters$Cluster <- cluster_assignments

# centroids (average values of each variable for each cluster)
centroids <- aggregate(. ~ Cluster, data = data_with_clusters, mean)
print(centroids)

### part d ####

set.seed(3)

#sample of 95% of the data
sample_data <- data[sample(1:nrow(data), size = 0.95 * nrow(data)), ]

# Normalize
sample_data.norm <- sapply(sample_data, scale)

dist.sample.norm <- dist(sample_data.norm, method = "euclidean")
hc2 <- hclust(dist.sample.norm, method = "ward.D")

#dendrogram of the sampled data
dend_sample <- as.dendrogram(hc2)
dend_sample <- color_branches(dend_sample, k = k)
plot(dend_sample, main = "Dendrogram with 95% Sample Data")

### part e ###

# run kmeans algorithm 
set.seed(2)
km <- kmeans(data.norm, 6)

# Clusters
print(km$cluster)
km_cluster <- (km$cluster)

# centroids
centroids <- km$centers

print(centroids)

# Create a color palette for 6 clusters
colors <- rainbow(6)
par(mar = c(9, 2, 0, 2))

#empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "n", 
     ylim = c(min(centroids), max(centroids)), xlim = c(0,11))

axis(1, at = 1:11, labels = names(data), las=2)  

for (i in 1:6) {lines(centroids[i, ], lty = 1, lwd = 2, col = colors[i])}

for (i in 1:6) {text(x = 0.5, y = centroids[i, 1], labels = paste("Cluster", i), 
                     col = colors[i])}














        