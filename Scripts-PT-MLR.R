#Counter terrorism project

mydata <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/MLR/PT-MLR.csv",header=TRUE,sep=",");

View(mydata)

head(mydata)

Q19_lm = lm(Q19~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q17+Q20+Q21+Q23+Q10+Q14+Q36+Q32,data=mydata)

coeffs=coefficients(Q19_lm);

coeffs

summary(Q19_lm)

# Multi linear regression equation

# Q19 = 0.0581571*Q2 + 0.0436326*Q7 + 0.0472010*Q29 + 0.0041248*Q55 + 0.0768400*Q20 + 0.2226406*Q21 +0.0852621Q23+0.0646799*Q10 + 0.0352530*Q36 - 0.0441856*Q32

# Pr value for Q2, Q7, Q29, Q55, Q20, Q21, Q23, Q10, Q36 and Q32 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q19

plot(mydata$Q19,mydata$Q2)

plot(mydata$Q19,mydata$Q7)

plot(mydata$Q19,mydata$Q29)

plot(mydata$Q19,mydata$Q55)

plot(mydata$Q19,mydata$Q20)

plot(mydata$Q19,mydata$Q21)

plot(mydata$Q19,mydata$Q23)

plot(mydata$Q19,mydata$Q10)

plot(mydata$Q19,mydata$Q36)

plot(mydata$Q19,mydata$Q32)

predict(Q19_lm)

confint(Q19_lm)

Q19.res = resid(Q19_lm)

Q19.res

plot(Q19_lm)

predict(Q19_lm, level = 0.95)

#scatter plots
pairs(~Q19+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q19+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q19_stdres=rstandard(Q19_lm)

#standard residual plot

qqnorm(Q19_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q19_stdres)
