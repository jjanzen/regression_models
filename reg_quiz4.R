Question 1
Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autolander 
as the outcome (variable name use). Fit a logistic regression model with autolander (variable auto) use 
(labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). 
Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the 
variable headwind (numerator) to tail winds (denominator).

library(MASS)
data(shuttle)
head(shuttle)
summary(shuttle)

shuttle_data <- shuttle
# convert use column from factor to numeric 
shuttle_data$use <- as.numeric(shuttle_data$use=='auto')
fit <- glm(use ~ factor(wind)-1, family = binomial, data = shuttle_data)
summary(fit)
exp(coef(fit))
factor(wind)head factor(wind)tail 
1.285714         1.327273 
1.286/1.327
# 0.96

0.031
1.327
-0.031
0.969

Question 2
Consider the previous problem. Give the estimated odds ratio for autolander use comparing head winds 
(numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.

shuttle_data <- shuttle
# convert use column from factor to numeric 
shuttle_data$use <- as.numeric(shuttle_data$use=='auto')
fit <- glm(use ~ factor(wind) + factor(magn)-1, family = binomial, data = shuttle_data)
summary(fit)$coef
exp(coef(fit))
> exp(coef(fit))
factor(wind)head   factor(wind)tail factor(magn)Medium    factor(magn)Out factor(magn)Strong 
1.4383682          1.4851533          1.0000000          0.6841941          0.9376181 

1.4383682/1.4851533
# 0.968
0.684
0.969
1.00
1.485

Question 3
If you fit a logistic regression model to a binary variable, for example use of the autolander, 
then fit a logistic regression model for one minus the outcome (not using the autolander) what happens 
to the coefficients?

The coefficients reverse their signs.  answer
The intercept changes sign, but the other coefficients dont
The coefficients change in a non-linear fashion.
The coefficients get inverted (one over their previous value).

Question 4
Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. 
Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).

data(InsectSprays)
insect_data <- InsectSprays
summary(insect_data)
head(insect_data)
fit <- glm(count~factor(spray)-1, data= insect_data, family=poisson)
summary(fit)$coef
exp(coef(fit))
factor(spray)A factor(spray)B factor(spray)C factor(spray)D factor(spray)E factor(spray)F 
14.500000      15.333333       2.083333       4.916667       3.500000      16.666667 
14.5/15.33
# 0945
0.136
0.9457
0.321
-0.056

Question 5
Consider a Poisson glm with an offset, t. So, for example, a model of the form glm(count ~ x + offset(t), 
family = poisson) where x is a factor variable comparing a treatment (1) to a control (0) and t is the natural 
log of a monitoring time. What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), 
family = poisson) where t2 <- log(10) + t? In other words, what happens to the coefficients if we change the 
units of the offset variable. (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)

fit <- glm(count~factor(spray), family=poisson, data= InsectSprays, offset = log(count+1))
summary(fit)$coef

fit2 <- glm(count~factor(spray), family=poisson, data= InsectSprays, offset = log(10) + log(count+1))
summary(fit2)$coef

The coefficient estimate is unchanged answer
The coefficient estimate is divided by 10.
The coefficient estimate is multiplied by 10.
The coefficient is subtracted by log(10).

Question 6
Consider the data

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0. 
Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?

knots<-c(0)
splineTerms<-sapply(knots,function(knot) (x>knot)*(x-knot))
xmat<-cbind(1,x,splineTerms)
fit<-lm(y~xmat-1)
yhat<-predict(fit)

summary(fit)$coef
yhat
(yhat[10]-yhat[6])/4
plot(x,y)
lines(x,yhat,col="red")

1.013
-1.024
2.037
-0.183
