#Counter terrorism project

#Victimology

mydata <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/MLR/VIC-MLR.csv",header=TRUE,sep=",");

View(mydata)

head(mydata)

Q49_lm = lm(Q49~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q51+Q33+Q34,data=mydata)

coeffs=coefficients(Q49_lm);

coeffs

summary(Q49_lm)

# Multi linear regression equation

# Q49 = 09568*Q51 + 0.0151*Q66 - 0.41442*Q53 - 0.255299*Q7 -0.171949*Q1

# Pr value for Q51, Q66, Q53, Q7 and Q1 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q49

plot(mydata$Q49,mydata$Q53)

plot(mydata$Q49,mydata$Q51)

plot(mydata$Q49,mydata$Q66)

plot(mydata$Q49,mydata$Q7)

plot(mydata$Q49,mydata$Q1)




predict(Q49_lm)

confint(Q49_lm)

Q49.res = resid(Q49_lm)

Q49.res

predict(Q49_lm, level = 0.95)



#scatter plots
pairs(~Q36+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q36+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q49_stdres=rstandard(Q49_lm)

#standard residual plot

qqnorm(Q49_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q49_stdres)
