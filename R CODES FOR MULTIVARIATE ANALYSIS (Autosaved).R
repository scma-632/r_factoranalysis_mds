setwd('/Users/lalithachoth/Desktop/Rlearn')
df = read.csv('winequalityN.csv',header=TRUE)
head(df)

dput(names(df))
dim(df)
any(is.na(df))
sum(is.na(df))

df1 <- na.omit(df, axis=1)
dim(df1)
any(is.na(df1))
summary(df1)

cor(df1[,c(2:13)])
heatmap(cor(df1[,c(2:13)]))

#TO PERFORM FACTOR ANALYSIS

wine.factor <- factanal(df1[,c(2:13)], factors = 5) # varimax is the default
wine.factor
print(wine.factor, digits=2, cutoff=0.25, sort=TRUE)
library(psych)
KMO(df1[,c(3:5,6:8,11,13)])
factanal(df1[,c(2:13)], factors = 5, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
scores = factanal(~., data=df1[,c(2:13)], factors = 5,scores = "Bartlett")$scores
dim(scores)
scores = data.frame(scores)

str(scores)
dim(df1)
pooled = cbind(df1,scores)      
names(pooled)


pooled$composite = 1.645*pooled$Factor1 + 1.639*pooled$Factor2+ 1.521*pooled$Factor3+ 1.438*pooled$Factor4 +1.352*pooled$Factor5
head(pooled)


#PERFORM CLUSTER ANALYSIS
summary(df1)
df2 = scale(df1[,c(2:13)])
head(df2)
describe(df2)

library(psych)
test.data <- df2
ic.out <- iclust(test.data,title="ICLUST of the Wine data")
summary(ic.out)

cl <- kmeans(df2, 5, nstart = 25)
## IGNORE_RDIFF_END
plot(df2, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


# Decide how many clusters to look at
# Initialize total within sum of squares error: wss

library(tidyverse)
# Load the necessary package
library(tibble)

n_clusters <- 10
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n_clusters possible clusters
for (i in 1:n_clusters) {
  km.out <- kmeans(df2, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Create a tibble with the results
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
    geom_point(size = 4)+
    geom_line() +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    xlab('Number of clusters')
scree_plot

set.seed(123)
km.out <- kmeans(df2, centers = 3, nstart = 20)
km.out
plot(km.out,centers = 3, nstart = 20)

==========
#install.packages('factoextra')
library(factoextra)
library(cluster)
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(df2, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df2, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(df2, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 3 clusters
groups <- cutree(final_clust, k=3)
plot(groups)
#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(df1, cluster = groups)

#display first six rows of final data
head(final_data)

#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), FUN=mean)

#MULTI DIMENSIONAL SCALING

#calculate distance matrix
d <- dist(df2)

#perform multidimensional scaling
fit <- cmdscale(d, eig=TRUE, k=3)

#extract (x, y) coordinates of multidimensional scaleing
x <- fit$points[,1]
y <- fit$points[,2]

#create scatter plot
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Multidimensional Scaling Results", type="n")

#add row names of data frame as labels
text(x, y, labels=row.names(df1))

# 3 D GRAPHS
# Install and load the scatterplot3d package
#install.packages("scatterplot3d")
library(scatterplot3d)

# Assuming df1 is already defined
# Calculate distance matrix
d <- dist(df1)

# Perform multidimensional scaling with 3 dimensions
fit <- cmdscale(d, eig=TRUE, k=3)

# Extract (x, y, z) coordinates of multidimensional scaling
x <- fit$points[,1]
y <- fit$points[,2]
z <- fit$points[,3]

# Create 3D scatter plot
s3d <- scatterplot3d(x, y, z, xlab="Coordinate 1", ylab="Coordinate 2", zlab="Coordinate 3",
                     main="Multidimensional Scaling Results")
dput(names(df))
# Add row names of data frame as labels
s3d$points3d(x, y, z, col="blue", pch=16)
text(s3d$xyz.convert(x, y, z), labels=row.names(df), cex=0.7, pos=4)


# Attach the df1 data frame
attach(df)

# Perform the aggregation
df2 <- aggregate(cbind(Alcohol, Malic_acid, Ash, Alcalinity_ash, 
                       Magnesium, Total_phenols, Flavanoids, Nonflavanoid_phenols, 
                       Proanthocyanins, Color_intensity, Hue, diluted_wines, 
                       Proline) ~ class, data=df1, FUN=mean)

# Detach the data frame after use
detach()
df3 = scale(df2)
#calculate distance matrix
d <- dist(df3)

#perform multidimensional scaling
fit <- cmdscale(d, eig=TRUE, k=2)

#extract (x, y) coordinates of multidimensional scaleing
x <- fit$points[,1]
y <- fit$points[,2]

#create scatter plot
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Multidimensional Scaling Results", type="n")

#add row names of data frame as labels
text(x, y, labels=row.names(df2))

