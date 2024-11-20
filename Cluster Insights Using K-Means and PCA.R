## Introduction: 

#The aim of the project is to perform clustering on the countries dataset to see any tendency of those countries.
#then, PCA dimension reduction method will be conducted to see how many variables are enough to explain the
#majority of information. After that, clustering will be performed again to see how the clusters changed


library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(cluster)
library(ClusterR)
library(cleandata)
library(gridExtra)




## 1. Data preparation

#The dataset was taken from kaggle (https://www.kaggle.com/datasets/fernandol/countries-of-the-world) with 
#variables like infant mortality, deathrate, birthrate, gpd per capita - all valuable information that will be 
#used for clustering 


Data<-read.csv("countries.csv")


#Let's separate character variables(country, region) from numeric variables (the rest)

Data2<-Data
Data2<-Data2%>%relocate(Climate, .after=Service)


# The dataset uses commas as decimal separators. We'll replace commas with dots 
# and convert character columns to numeric for clustering.

replace_commas <- function(column) {
  gsub(",", ".", column)
}

Data3 <- Data2 %>% 
  mutate(across(where(is.character), replace_commas))

convert_chr_to_numeric <- function(column) {
  if(is.character(column)) {
    as.numeric(column)
  } else {
    column
  }
}


Data4 <- as.data.frame(lapply(Data3[3:20], convert_chr_to_numeric))


# Add back the country and region columns for easier interpretation during the analysis.

Data4 <- cbind(Data4,Data3[1:2])


# Remove rows with missing values to ensure clean and complete data for clustering.

Data4<-na.omit(Data4)


# Standardize numerical variables (z-scaling) for uniform scaling, 
# excluding the "Climate" column, which has categorical values ranging from 0 to 4.

Data5<-scale(Data4[1:17])

# Add the "Climate" column back to the scaled dataset. 
# "Country" and "Region" are excluded as they are non-numeric.

Data5<-cbind(Data5,Data4$Climate)


#The data has been prepared.

## 2. Clustering

# Before clustering, we'll evaluate the dataset's clustering tendency using Hopkins statistic 
# and determine the optimal number of clusters using the elbow and silhouette methods.

### Calculating hopkin's statistics

get_clust_tendency(Data5,2,graph=TRUE)


# The Hopkins statistic is 0.87, indicating a strong tendency for clusterability. This is promising.

### Silhouette method

opt_silhouette<-Optimal_Clusters_KMeans(Data5, max_clusters=10, plot_clusters=TRUE, criterion="silhouette")


### Elbow method


opt_elbow<-Optimal_Clusters_KMeans(Data5, max_clusters=10, plot_clusters = TRUE)



# Both the silhouette and elbow methods suggest that 2 clusters are optimal.

#We're ready to perform clustering
#We will use the k-means clustering

### K-means 


cluster_km<-eclust(Data5,FUNcluster="kmeans",2)


### Silhouette width 


fviz_silhouette(cluster_km)



#Average Silhouette is equal to 0.23, which indicates low clustering quality. 



#Let's see what story the clusters can tell us, we will choose 10 random countries and regions, 
#some of their characteristics and their clusters 


Data4$cluster<-cluster_km$cluster
Data_not_scaled<-Data4
Data_not_scaled[sample(nrow(Data_not_scaled),10),c(1, 3, 13, 14,19,20,21)]

# Based on the results, Cluster 2 corresponds mostly to developed countries with lower death rates 
# and infant mortality, and higher literacy rates. Cluster 1 generally represents developing or poorer countries.

#Now, let's perform PCA now and perform the same K-means clustering on it

# 3.Principal Component Analysis


# Principal Component Analysis (PCA) reduces the number of dimensions while retaining most of the information, 
# addressing the curse of dimensionality.

pca <- prcomp(Data5, center=FALSE, scale=FALSE)


# Check the proportion of variance explained by the principal components.


summary(pca)


### Scree plot


fviz_eig(pca, choice='eigenvalue')


### Now, the scree plot with percentages


fviz_eig(pca)


fviz_pca_var(pca)
 
# The first two principal components explain only 47.3% of the variance, which is low. 
# We'll apply the Kaiser rule to identify how many components explain the majority of the dataset's variance.

### Kaiser rule

#We will now see the eigenvalues of our dataset.# According to the Kaiser rule,
# components with eigenvalues of 1 or higher are retained.

Data5.cov<-cov(Data5)
Data5.eigen<-eigen(Data5.cov) 
Data5.eigen$value

#Only those above 1 should be chosen. According to the output PC1 to PC5 are retained.

#In our case, the first 5 components explain 75% of the variance



PC1 <- fviz_contrib(pca, choice = "var", axes = 1)
PC2 <- fviz_contrib(pca, choice = "var", axes = 2)
PC3 <- fviz_contrib(pca, choice = "var", axes = 3)
PC4 <- fviz_contrib(pca, choice = "var", axes = 4)
PC5 <- fviz_contrib(pca, choice = "var", axes = 5)
grid.arrange(PC1, PC2)
grid.arrange(PC3, PC4)
grid.arrange(PC5)

# From the contribution plots for Dim-1 and Dim-2, infant mortality per 1000 births 
# is the leading variable, contributing ~36% to Dim-1 and ~60% to Dim-2. 
# Other variables like birthrate, phones per 1000 people, literacy, GDP per capita, 
# and agriculture also contribute, but to a lesser extent.



## 4. performing clustering on PCA'd dataset


pca_after<-prcomp(Data5,center=FALSE, scale.=FALSE, rank. = 5)

# Repeat the previous steps of calculating the Hopkins statistic, and determining 
# the optimal number of clusters using the silhouette and elbow methods.

### Hopkins statistics


get_clust_tendency(pca_after$x,2,graph=TRUE)


# The Hopkins statistic is 0.59, significantly lower than before (0.87) and closer to 0.5, 
# indicating weaker clustering tendency.

### Silhouette PCA


opt_silhouette_pca<-Optimal_Clusters_KMeans(pca_after$x, max_clusters=10, plot_clusters=TRUE, criterion="silhouette")


### Elbow PCA


opt_elbow_pca<-Optimal_Clusters_KMeans(pca_after$x, max_clusters=10, plot_clusters = TRUE)


#Based on the elbow and silhouette methods, we will choose 2 clusters again

### Perform K-means on PCA'd dataset

cluster_pca_kmeans<-eclust(pca_after$x,FUNcluster="kmeans",2)

fviz_silhouette(cluster_pca_kmeans)



# The average silhouette width is higher (0.32) than the non-PCA counterpart (0.23). 
# However, the lower Hopkins statistic and the high number of principal components required 
# (5 PCs explain only 75% of the variance) suggest that PCA may not be the best dimensional reduction 
# method for this dataset.


## 5.Summary

# Clustering revealed tendencies such as a division into poorer/developing countries (Cluster 1) 
# and developed/richer countries (Cluster 2). Despite a strong Hopkins statistic (0.87), 
# the average silhouette width (0.23) was low, indicating limited clustering quality. 
# Dimension reduction via PCA slightly improved silhouette width (0.32), but the reduced Hopkins statistic 
# and the need for multiple components to explain the variance suggest that PCA may not be 
# the most suitable dimensional reduction method for this dataset.

## 6.Sources

# https://www.datanovia.com/en/lessons/assessing-clustering-tendency/

# https://sanchitamangale12.medium.com/scree-plot-733ed72c8608 

# https://towardsdatascience.com/curse-of-dimensionality-an-intuitive-exploration-1fbf155e1411

# ChatGPT  (converting chr to numeric function and replace commas function)
