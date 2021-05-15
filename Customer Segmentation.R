customer_data=read.csv("D:/AI Machine Deep Learning Coureses/Customer-Segmentation_R_Project/Mall_Customers.csv")

#--------------------------------------------------------------------------------------#
str(customer_data)

#--------------------------------------------------------------------------------------#
names(customer_data)

#--------------------------------------------------------------------------------------#
head(customer_data)

#--------------------------------------------------------------------------------------#
summary(customer_data$Age)

#--------------------------------------------------------------------------------------#
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)

#--------------------------------------------------------------------------------------#
a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
       ylab="Count",
       xlab="Gender",
       col=rainbow(2),
       legend=rownames(a))

#--------------------------------------------------------------------------------------#
a=table(customer_data$Gender)
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
   main="Pie Chart Depicting Ratio of Female and Male")

#--------------------------------------------------------------------------------------#
a=table(customer_data$Gender)
hist(customer_data$Age,
    col="blue",
    main="Histogram to Show Count of Age Class",
    xlab="Age Class",
    ylab="Frequency",
    labels=TRUE)

#--------------------------------------------------------------------------------------#
boxplot(customer_data$Age,
       col="red",
       main="Boxplot for Descriptive Analysis of Age")

#--------------------------------------------------------------------------------------#
# Analysis of the Annual Income of the Custormers

summary(customer_data$Annual.Income..k..)

hist(customer_data$Annual.Income..k.., col = "#660033", main = "Histogram for Annual Income", xlab = "Annual Income Class",
     ylab = "Frequency", labels = TRUE)

#--------------------------------------------------------------------------------------#
plot(density(customer_data$Annual.Income..k..),col = "yellow",main = "Density Plot for Annual Income",xlab = "Annual Income Class",
     ylab = "Density")
polygon(density(customer_data$Annual.Income..k..),col = "#ccff66")

#--------------------------------------------------------------------------------------#
# Analysis Spending Score of the Customers

boxplot(customer_data$Spending.Score..1.100., horizontal = TRUE, col = "#990000",main="BoxPlot for Descriptive Analysis
of Spending Score")

#--------------------------------------------------------------------------------------#
hist(customer_data$Spending.Score..1.100., main = "HistoGram for Spending Score", xlab = "Spending Score Class",
     ylab = "Frequency", col = "#6600cc", labels = TRUE)

#--------------------------------------------------------------------------------------#
# K-Means Algorithm

library(purrr)
set.seed(123)

iss = function (k){
  kmeans(customer_data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}
k.values = 1:10

iss_values = map_dbl(k.values, iss)
plot(k.values, iss_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Total intra-clusters sum of squares")

#--------------------------------------------------------------------------------------#
library(purrr)
library(cluster)
library(gridExtra)
library(grid)

set.seed(123)
k2 = kmeans(customer_data[,3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s2 = plot(silhouette(k2$cluster, dist(customer_data[,3:5],"euclidean")))

#--------------------------------------------------------------------------------------#

# Visualizing the Clustering Results using the First Two Priciple Componets

pcclust = prcomp(customer_data[,3:5], scale = FALSE )
summary(pcclust)

pcclust$rotation[,1:2]

#--------------------------------------------------------------------------------------#