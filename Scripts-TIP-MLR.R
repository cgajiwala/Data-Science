#Counter terrorism project - Trust in Police component

mydata <- read.csv("C:/Users/atuln/Desktop/ADS Final Project Doc/MLR/TIP-MLR.csv",header=TRUE,sep=",");

View(mydata)

head(mydata)

Q8_lm = lm(Q8~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q9+Q10+Q12+Q13+Q40+Q41+Q20+Q34+Q35,data=mydata)

coeffs=coefficients(Q8_lm);

coeffs

summary(Q8_lm)

# Multi linear regression equation

# Q8 = 0.2854*Q1 + Q2*0.41317 + Q3*1.243  + Q5*0.4856 + Q6*1.19 + Q7*0.49 - Q24*0.7403 + Q53*0.844 + Q9*1.11688 + Q10*0.412 - Q12*0.468 + Q13*0.9359 + Q20*0.570 + Q34*0.506 + Q35*0.316

# Pr value for Q1, Q2, Q3, Q5, Q6, Q7, Q24, Q53, Q9, Q10, Q12, Q13, Q20, Q34  and Q35 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q8

plot(mydata$Q8,mydata$Q1)

plot(mydata$Q8,mydata$Q2)

plot(mydata$Q8,mydata$Q3)

plot(mydata$Q8,mydata$Q5)

plot(mydata$Q8,mydata$Q6)

plot(mydata$Q8,mydata$Q7)

plot(mydata$Q8,mydata$Q24)

plot(mydata$Q8,mydata$Q53)

plot(mydata$Q8,mydata$Q9)

plot(mydata$Q8,mydata$Q10)

plot(mydata$Q8,mydata$Q12)

plot(mydata$Q8,mydata$Q13)

plot(mydata$Q8,mydata$Q20)

plot(mydata$Q8,mydata$Q34)

plot(mydata$Q8,mydata$Q35)


predict(Q8_lm)

confint(Q8_lm)

Q8.res = resid(Q8_lm)

Q8.res

plot(Q8_lm)

predict(Q8_lm, level = 0.95)

#scatter plots
pairs(~Q36+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q36+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q8_stdres=rstandard(Q8_lm)

#standard residual plot

qqnorm(Q8_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q8_stdres)
