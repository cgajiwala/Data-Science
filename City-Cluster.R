City_Status_Result <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/Test3 - Copy.csv",header=TRUE,sep=",");

View(City_Status_Result)

City_Status_Result_Data<- City_Status_Result



City_Status_Result_Data$City <- NULL

(kmeans.City_Status_Result <- kmeans(City_Status_Result_Data,5))

City_Status_Result2 <- City_Status_Result

View(City_Status_Result2)

table(City_Status_Result2$City,kmeans.City_Status_Result$cluster)

plot(City_Status_Result2[c("Trust.in.the.Police","Cooperation.with.the.Police")],col=kmeans.City_Status_Result$cluster)



points(kmeans.City_Status_Result$centers[,c("Trust.in.the.Police","Cooperation.with.the.Police")],col=1:20,pch=16,cex=2)

# Clustering using pam commanfd.

library(fpc)
library(cluster)

City_Status_Result_Data1 <- City_Status_Result

pam.City_Status_Result_Data1<- pam(City_Status_Result_Data1,2)

table(pam.City_Status_Result_Data1$clustering,City_Status_Result$City)

layout(matrix(c(1,2),1,2))

plot(pam.City_Status_Result_Data1)


# # Hierarchical Clustering for city
# 
# 
# City_Status_Result_Data2 <- City_Status_Result
# 
# City_Status_Result_Data2$City <- NULL
# 
# hc <- hclust(dist(City_Status_Result_Data2),method="med")
# 
# plot(hc,hang=-1,label=City_Status_Result$City)
# 
# rect.hclust(hc,k=5)
# 
# groups <- cutree(hc,k=5)
