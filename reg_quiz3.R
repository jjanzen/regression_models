Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable 
and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
require(datasets)
data(mtcars)
summary(lm(mtcars$wt ~ mtcars$cyl, data = mtcars))$coefficients

fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)

33.991
-4.256
-3.206
-6.071 answer

Question 2
Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable 
and weight as a possible confounding variable. Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and 
unadjusted by weight models. Here, adjusted means including the weight variable as a term in the regression model and 
unadjusted means the model without weight included. What can be said about the effect comparing 8 and 4 cylinders after 
looking at models with and without weight included?.

fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit1)

fit2 <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit2)

Including or excluding weight does not appear to change anything regarding the estimated impact of number of cylinders on mpg.
Holding weight constant, cylinder appears to have more of an impact on mpg than if weight is disregarded.  answer
Within a given weight, 8 cylinder vehicles have an expected 12 mpg drop in fuel efficiency.
Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

Question 3
Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable 
and weight as confounder. Now fit a second model with mpg as the outcome model that considers the interaction between number 
of cylinders (as a factor variable) and weight. Give the P-value for the likelihood ratio test comparing the two models and 
suggest a model using 0.05 as a type I error rate significance benchmark.

fit3 <- lm(mpg ~ factor(cyl)*wt, mtcars)
summary(fit3)

compare <- anova(fit1, fit3)
compare
compare$Pr

The P-value is small (less than 0.05). Thus it is surely true that there is no interaction term in the true model.
The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms is necessary. wrong answer
The P-value is small (less than 0.05). So, according to our criterion, we reject, which suggests that the interaction term is necessary
The P-value is small (less than 0.05). Thus it is surely true that there is an interaction term in the true model.
The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary.
The P-value is small (less than 0.05). So, according to our criterion, we reject, which suggests that the interaction term is not necessary.

Question 4
Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and 
weight inlcuded in the model as

lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
How is the wt coefficient interpretted?

The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8).
The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8). answer
The estimated expected change in MPG per half ton increase in weight for the average number of cylinders.
The estimated expected change in MPG per one ton increase in weight.
The estimated expected change in MPG per half ton increase in weight.

Question 5
Consider the following data set

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
Give the hat diagonal for the most influential point

fit <- lm(y ~ x)
summary(fit)
hatvalues(fit)

0.2804
0.9946 answer
0.2287
0.2025

Question 6
Consider the following data set

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
Give the slope dfbeta for the point with the highest hat value.

fit <- lm(y ~ x)
hatvalues(fit)
dfbetas(fit)

-0.378
-.00134
-134 answer
0.673

Question 7
Consider a regression relationship between Y and X with and without adjustment for a third variable Z. Which of the 
following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.

For the the coefficient to change sign, there must be a significant interaction term.
Adjusting for another variable can only attenuate the coefficient toward zero. It can't materially change sign.
It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment. answer
The coefficient can't change sign after adjustment, except for slight numerical pathological cases.

