#Counter terrorism project Public Expectations

mydata <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/MLR/PE-MLR.csv",header=TRUE,sep=",");

View(mydata)

head(mydata)

Q40_lm = lm(Q40~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q41,data=mydata)

coeffs=coefficients(Q40_lm);

coeffs

summary(Q40_lm)

# Multi linear regression equation

# Q40 = -0.689367*Q30 + 0.139270*Q55 + 0.092795*Q59 + 0.040189*Q60 - 0.082948*Q63 + 0.417473*Q41

# Pr value for Q30, Q55, Q59, Q60, Q63 and Q41 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q40

plot(mydata$Q40,mydata$Q30)

plot(mydata$Q40,mydata$Q55)

plot(mydata$Q40,mydata$Q59)

plot(mydata$Q40,mydata$Q60)

plot(mydata$Q40,mydata$Q63)

plot(mydata$Q40,mydata$Q41)


predict(Q40_lm)

confint(Q40_lm)

Q40.res = resid(Q40_lm)

Q40.res

plot(Q40_lm)

predict(Q40_lm, level = 0.95)

#scatter plots
pairs(~Q40+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q40+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q40_stdres=rstandard(Q40_lm)

#standard residual plot

qqnorm(Q40_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q40_stdres)
