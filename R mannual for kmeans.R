#STEP 1: import file
suicide_rate <- read.csv("E:/Self-Learning/Data Analytics/DATA/mental health & suicide rate/u1_Crude suicide rates.csv")
View(suicide_rate)

#STEP 2: Scale to standardise 
scaled.suicide_rate <- scale(suicide_rate[c(-1,-2)])

#STEP 3: Find optimal no. of clusters using NBClust
#3.1 FOR INSTALL: install and load the NbClust package
install.packages("NbClust")
library(NbClust)

#3.2 SET SEED: use NbClust to determine the optimal number of clusters
set.seed(248)
nc <- NbClust(scaled.suicide_rate[c(-1,-2)], min.nc = 2, max.nc = 4, method = "kmeans")


#3.3 PERFORM KMEANS: perform k-means clustering with the optimal number of clusters
kmeans.clus <- kmeans(scaled.suicide_rate, centers = 4, nstart = 25)
kmeans.clus


#STEP 4: PLOT THE CLUSTERS
#4.1: USE "fpc" PACKAGE
library(fpc)
plotcluster(scaled.suicide_rate, kmeans.clus$cluster)

#STEP 5: PROFILING THE CLUSTERS
#5.1 CLUSTER PROFILE: Aggregate Cluster Profiles
suicide_rate$Cluster <- kmeans.clus$cluster
aggr <- aggregate(suicide_rate[, c(3 ,4:15 )], by = list(suicide_rate$Cluster), mean)

#5.2 PROFILE SUMMARY: Create Cluster Profile Summary
clus.profile <- data.frame(Clusters = aggr[, 1], 
                           Freq = as.vector(table(suicide_rate$Cluster)),
                           aggr[, -1])

#5.3 Print the cluster profile
print(clus.profile)


#EXPORT 
library(writexl)

# Assuming your cluster profile data is stored in the variable 'clus.profile'

# Define the file path and name for the Excel file
kmeans_output <- "E:/Self-Learning/Data Analytics/DATA/mental health & suicide rate/kmean_output.xlsx"

# Write the data to an Excel file
write.xlsx(clus.profile, file = kmeans_output, sheetName = "kmean_output", colNames = TRUE, rowNames = TRUE, append = FALSE)

###REPEAT FROM STEP 3.3 FOR 3 CLUSTERS