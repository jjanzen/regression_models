data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
coef(lm(y~x))
# (Intercept)           x 
# 37.285126          -5.344472 

Question 1
Consider the following data with x as the predictor and y as as the outcome.

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.

plot(x,y)
abline(fit,col="red")
fit <- lm(y~x)
est <- predict(fit,data.frame(x))
coef(lm(y~x))
summary(fit)

Residuals:
    Min       1Q   Median       3Q      Max 
-0.27636 -0.18807  0.01364  0.16595  0.27143 

Coefficients:
    Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.1885     0.2061   0.914    0.391  
x             0.7224     0.3107   2.325    0.053 .
---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.223 on 7 degrees of freedom
Multiple R-squared:  0.4358,    Adjusted R-squared:  0.3552 
F-statistic: 5.408 on 1 and 7 DF,  p-value: 0.05296

2*pt(-abs(2.325),df=length(x)-1)
# 0.049

2.325
0.025
0.05296 # answer
0.391

Question 2
Consider the previous problem, give the estimate of the residual standard deviation.

0.05296
0.3552
0.223 # answer
0.4358

Question 3
In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg 
at the average weight. What is the lower endpoint?

data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg

avg_weight <- mean(mtcars$wt)
fit <- lm(y~x)
coef(lm(y~x))
summary(fit)
abline(fit,col="red")
predict(fit,data.frame(x=mean(x)),interval = "confidence")
# fit      lwr      upr
#20.09062 18.99098 21.19027
p1 <- predict(fit,data.frame(x),interval = "confidence")
plot(x,y,xlab='Weight (1000lb)',ylab='MPG')
abline(fit,col="red")
lines(x,p1[,2],col="purple")
lines(x,p1[,3],col="purple")
class(x)
21.190
18.991 answer
-4.00
-6.486

Question 5
Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs). 
A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint?

data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y~x)
predict(fit,data.frame(x=(3000/1000)), interval="prediction")

27.57 answer 
14.93
21.25
-5.77

Question 6
Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). 
A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. 
Give the lower endpoint.

data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y~x)
coef(lm(y~x))
summary(fit)

fit2<-lm(y~I(x/2))
?lm
summary(fit2)
tbl2<-summary(fit2)$coefficients
tbl2
mn<-tbl2[2,1]      #mean is the estimated slope
mn
std_err<-tbl2[2,2] #standard error
std_err
deg_fr<-fit2$df    #degree of freedom
#Two sides T-Tests
mn + c(-1,1) * qt(0.975,df=deg_fr) * std_err
# -12.97262  -8.40527

4.2026
-6.486
-9.000
-12.973 answer

Question 7
If my X from a linear regression is measured in centimeters and I convert it to meters what would happen to the slope coefficient?

It would get multiplied by 10
It would get divided by 100 anser
It would get divided by 10
It would get multiplied by 100.

Question 8
I have an outcome, Y, and a predictor, X and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1. 
What would be the consequence to the subsequent slope and intercept if I were to refit the model with a new regressor, X+c for some constant, c?

c <- 5
cf1 <- summary(fit)$coefficients
cf1

fit4 <- lm(y~I(x+c))
cf2 <- summary(fit4)$coefficients
cf2

The new intercept would be β^0−cβ^1
The new intercept would be β^0+cβ^1
The new slope would be β^1+c
The new slope would be cβ^1

Question 9
Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. About what is the ratio of the the 
sum of the squared errors, ∑ni=1(Yi−Y^i)2 when comparing a model with just an intercept (denominator) to the model
with the intercept and slope (numerator)?

fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")
anova(fit)
anova(fit5)
278/1126
0.25 anser
0.75
0.50
4.00

Question 10
Do the residuals always have to sum to 0 in linear regression?

The residuals never sum to zero.
The residuals must always sum to zero.
If an intercept is included, the residuals most likely won't sum to zero.
If an intercept is included, then they will sum to 0.


