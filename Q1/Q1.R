library(ggplot2)
library(ggvis)
library(tidyverse)
library(lubridate)
library(dplyr)
library(feasts)
library(tsibble)
library(MASS)
library(reshape2)
library(reshape)
library(cowplot)
library(knitr)
library(corrplot)
library(cluster)

########################### Data Exploration ###########################
rm(list = ls())
my_data = read.csv("ingredient.csv")

head(my_data)


#check the dimension of the data 
dim(my_data)

#check for any null value 
is.null(my_data)

#Calcualte descriptive stat of data 
summary(my_data)

####################### 1a##############################

#Correlation matrix
corr_mat <- cor(my_data[, c(1,2,3,4,5,6,7,8,9)])
corrplot(corr_mat, method = "circle")

#ANOVA test
anova_ag <- aov(a+g ~ ., data = my_data)
summary(anova_ag)

####################### 1b##############################
#add id to data
my_data$id <- (1:214)

#reshape data 
reshape_data <- melt(my_data, id.vars = c("id"))

#histogram
ggplot(reshape_data, aes(x=value, fill=variable)) + geom_histogram(binwidth=.5, alpha=.5, position="identity")

#boxplot
ggplot(reshape_data, aes(x=variable, y=value, fill=variable)) + geom_boxplot()  + coord_flip()

#standard deviaiton 
sd(my_data$a)
sd(my_data$h)
sd(my_data$i)
sd(my_data$e)
sd(my_data$c)


####################### 1c##############################
library(caret)
library(factoextra)
library(viridis)
library(hrbrthemes)

data = read.csv("ingredient.csv")

#Min-Max scaling
process <- preProcess(data, method = c("range"))
norm_scale <- predict(process, data)


#Calculate avg silhoutte score 
# Note: try more starts to make clustering more stable 
i_silhouette_score <- function(k){
  km <- kmeans(norm_scale[,1:9], k, nstart = 50)
  ss <- silhouette(km$cluster, dist(norm_scale[,1:9]))
  mean(ss[,3])
  
}


#plot average silhoutte score for 1-10 clusters 
k <- 2:10
avg_sil <- sapply(k, i_silhouette_score)

plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette
Scores', main = "Average Silhouette Score")

#Elbow method 
fviz_nbclust(norm_scale, kmeans, method = "wss") + labs(subtitle = "Elbow Method")


#fit k means with k=3
kfit_3 <- kmeans(norm_scale[,1:9], 3, nstart = 50)

#fit k means with k=5
kfit_5 <- kmeans(norm_scale[,1:9], 5, nstart = 50)


#Reshape the data to sequentially arrange according to each additive
kfit.clusters <- kfit_3$cluster
norm_scale$Attribute <- kfit_3.clusters
data_norm <- melt(norm_scale, id.vars = c("Attribute"))
colnames(data_norm) <- c("Cluster", "Additives", "Value")



#Group by with cluster and addtivies and calcualte mean
df_grp <- data_norm %>% group_by(Cluster, Additives) %>% summarise(mean_val <- mean(Value))
colnames(df_grp) <- c("Cluster", "Additives", "Value")

#Visualize the clustering result 
ggplot(df_grp, aes(x=factor(Additives), y=Value, group = Cluster, color = Cluster)) + geom_line() + xlab("Additives")

