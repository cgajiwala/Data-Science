Gender_Status_Result <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/Test2 - Copy.csv",header=TRUE,sep=",");

View(Gender_Status_Result)

Gender_Status_Result_Data<- Gender_Status_Result



Gender_Status_Result_Data$Gender <- NULL

(kmeans.Gender_Status_Result <- kmeans(Gender_Status_Result_Data,5))

Gender_Status_Result2 <- Gender_Status_Result

View(Gender_Status_Result2)

table(Gender_Status_Result2$Gender,kmeans.Gender_Status_Result$cluster)

plot(Gender_Status_Result2[c("Deterrence","Victimology")],col=kmeans.Gender_Status_Result$cluster)



points(kmeans.Gender_Status_Result$centers[,c("Deterrence","Victimology")],col=1:20,pch=16,cex=2)


# Clustering using pam commanfd.

library(fpc)
library(cluster)

Gender_Status_Result_Data1 <- Gender_Status_Result

pam.Gender_Status_Result_Data1<- pam(Gender_Status_Result_Data1,10)

table(pam.Gender_Status_Result_Data1$clustering,Gender_Status_Result$Gender)

layout(matrix(c(1,2),1,2))

plot(pam.Gender_Status_Result_Data1)