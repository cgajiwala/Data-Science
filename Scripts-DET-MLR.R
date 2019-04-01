#Counter terrorism project

mydata <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/MLR/DET-MLR.csv",header=TRUE,sep=",");

View(mydata)

head(mydata)

Q35_lm = lm(Q35~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q32+Q33+Q34+Q20+Q21+Q38+Q8+Q13,data=mydata)

coeffs=coefficients(Q35_lm);

coeffs

summary(Q35_lm)

# Multi linear regression equation

# Q35 = 0.1289601*Q32 + 0.1033024*Q33 + 0.3381465*Q34 -0.0626307*Q21 + 0.0581981*Q6 + 0.0593732*Q3

# Pr value for Q32, Q33, Q34, Q21, Q6 and Q3 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q35

plot(mydata$Q35,mydata$Q32)

plot(mydata$Q35,mydata$Q33)

plot(mydata$Q35,mydata$Q34)

plot(mydata$Q35,mydata$Q21)

plot(mydata$Q35,mydata$Q6)

plot(mydata$Q35,mydata$Q3)


predict(Q35_lm)

confint(Q35_lm)

Q35.res = resid(Q35_lm)

Q35.res

plot(Q35_lm)

predict(Q35_lm, level = 0.95)

#scatter plots
pairs(~Q35+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q35+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q35_stdres=rstandard(Q35_lm)

#standard residual plot

qqnorm(Q35_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q35_stdres)
