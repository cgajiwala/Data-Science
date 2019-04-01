Marital_Status_Result <- read.csv("Test1 - Copy.csv",header=TRUE)

attach(Marital_Status_Result)

View(Marital_Status_Result)

Marital_Status_Result_Data<- Marital_Status_Result



Marital_Status_Result_Data$Marital.Status <- NULL

(kmeans.Marital_Status_Result <- kmeans(Marital_Status_Result_Data,5))

Marital_Status_Result2 <- Marital_Status_Result

View(Marital_Status_Result2)

table(Marital_Status_Result2$Marital.Status,kmeans.Marital_Status_Result$cluster)

plot(Marital_Status_Result2[c("Trust.in.the.Police","Deterrence")],col=kmeans.Marital_Status_Result$cluster)



points(kmeans.Marital_Status_Result$centers[,c("Trust.in.the.Police","Deterrence")],col=1:20,pch=16,cex=2)


# Clustering using pam commanfd.

library(fpc)
library(cluster)

Marital_Status_Result_Data1 <- Marital_Status_Result

pam.Marital_Status_Result_Data1<- pam(Marital_Status_Result_Data1,5)

table(pam.Marital_Status_Result_Data1$clustering,Marital_Status_Result$Marital.Status)

layout(matrix(c(1,2),1,2))

plot(pam.Marital_Status_Result_Data1)
