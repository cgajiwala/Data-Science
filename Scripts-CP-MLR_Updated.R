#Counter terrorism project

mydata <- read.csv("CP-MLR.csv",header=TRUE);

attach(mydata)

View(mydata)

head(mydata)

Q36_lm = lm(Q36~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q37+Q38+Q39+Q19+Q20+Q22+Q10+Q34,data=mydata)

anova_tst_old = aov(Q36~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q25+Q26+Q27+Q28+Q29+Q30+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70+Q37+Q38+Q39+Q19+Q20+Q22+Q10+Q34,data=mydata)

summary(anova_tst_old)

coeffs=coefficients(Q36_lm);

coeffs

summary(Q36_lm)

# Multi linear regression equation

# Q36 = 0.1129719*Q53 + 0.0241645*Q70 + 0.1717494*Q37 +  0.2531715*Q38 + 0.2520027*Q39 -0.0662307*Q20
#+ 0.1981727*Q10

#values for dependent variable. R square measures what your
#r square shud be close to 1

linear_model_upd <- lm (Q36~Q53+Q70+Q37+Q38+Q39+Q20+Q10)

summary(linear_model_upd)

anovatest = aov(Q36~Q53+Q70+Q37+Q38+Q39+Q20+Q10)

summary(anovatest)

#get rid of outliers--rows, observations.

# Pr value for Q53, Q70, Q37, Q38, Q39, Q20 and Q10 is less than 0.05, and hence are better
# contributors than others.

# Plotting each component against Q36

plot(mydata$Q36,mydata$Q53)

plot(mydata$Q36,mydata$Q70)

plot(mydata$Q36,mydata$Q37)

plot(mydata$Q36,mydata$Q38)

plot(mydata$Q36,mydata$Q39)

plot(mydata$Q36,mydata$Q20)

plot(mydata$Q36,mydata$Q10)

predict(Q36_lm)

confint(Q36_lm)

Q36.res = resid(Q36_lm)

Q36.res

plot(Q36_lm)

predict(Q36_lm, level = 0.95)

#scatter plots
pairs(~Q36+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q24+Q25+Q26+Q27+Q28+Q29+Q30,data=mydata,main="scatter plot")

pairs(~Q36+Q53+Q54+Q55+Q56+Q57+Q59+Q60+Q63+Q64+Q65+Q66+Q69+Q70,data=mydata,main="scatter plot")

Q36_stdres=rstandard(Q36_lm)

#standard residual plot

qqnorm(Q36_stdres,ylab = "Standardized Residuals", xlab = "Normal Scores", main = "standard residuals")

qqline(Q36_stdres)
