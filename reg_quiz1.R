install.packages("UsingR")
library(UsingR)
Question 1
Consider the data set given below

x <- c(0.18, -1.54, 0.42, 0.95)
And weights given by

w <- c(2, 1, 3, 1)
Give the value of μ that minimizes the least squares equation ∑ni=1 wi(xi−μ) ^ 2
w[4] * (x[4] - mu[4])^2

mu <- 0.3 #3.88
mu <- 1.077 #9.77
mu <- 0.1471#3.72
mu <- 0.0025 #3.86
sum_values <- 0
    for (i in 1:4) {
    zz <- w[i]  * (x[i] - mu)^2 
    sum_values <- zz + sum_values
    }
print (sum_values)

q1
0.300
1.077
0.1471 # answer
0.0025

Question 2
Consider the following data set

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
Fit the regression through the origin and get the slope treating y as the outcome and x as the regressor. 
(Hint, do not center the data since we want regression through the origin, not through the means of the data.)

coef(lm(y~x-1))
# 0.83
plot(x,y, pch=19, col="blue")

slope <- (y[10] - y[1])/(x[10] - x[1])

library(manipulate)
myPlot <- function(beta){
    freqData <- as.data.frame(table(x,y))
    names(freqData) <- c("child", "parent", "freq")
    plot(
        as.numeric(as.vector(y)),
        as.numeric(as.vector(x)),
        pch =21, col = "black", bg = "lightblue",
        cex = 0.15 * freqData$freq,
        xlab = "parent",
        ylab = "child"
        )
    abline(0, beta, lwd = 3)
    points(0,0,cex = 2, pch = 19)
    mse <- mean((y - beta * x)^2)
    title(paste("beta = ", beta, "mse = ", round(mse,3)))
}
manipulate(myPlot(beta), beta = slider(0.5, 1.2, step = 0.02))
# slider()
0.59915
-0.04462
-1.713
0.8263 # answer

Question 3
Do data(mtcars) from the datasets package and fit the regression model with mpg as the outcome and weight as the 
predictor. Give the slope coefficient.

data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
coef(lm(y~x))
# (Intercept)           x 
# 37.285126          -5.344472 
plot(x,y)
abline(lm(y~x),col="red")
-9.559
0.5591
30.2851
-5.344 answer

Question 4
Consider data with an outcome (Y) and a predictor (X). The standard deviation of the predictor is one half that of 
the outcome. The correlation between the two variables is .5. What value would the slope coefficient for the 
regression model with Y as the outcome and X as the predictor?
x <- c(2,3,4)
y <- c(1,3.0,2)
cor(x,y) #0.5
sd(x)
sd(y)
?sd
slope <- (y[3] - y[1])/(x[3] - x[1])
slope #0.5

coef(lm(y~x))
#(Intercept)           x 
#2.56395e-16 5.00000e-01 
3
0.25
1 # answer
4

Question 5
Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1. 
The correlation between the scores on the two tests was 0.4. What would be the expected score on Quiz 2 for a 
student who had a normalized score of 1.5 on Quiz 1?

mean <- 0 and 
variance <- 1 

Corr(Y,X)=0.4 (correlationship of coefficient)

if x is 1.5 , then y?

1.5 * 0.4 # 0.6

0.16
0.4
1.0
0.6 answer

Question 6
Consider the data given by the following

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?
mean(x) #9.31
sd(x) #0.75
xc <- (x-mean(x))/sd(x)
xc # -0.9718658  1.5310215 -0.3993969  0.4393366 -0.5990954

8.58
-0.9719 # answer
8.86
9.31

Question 7
Consider the following data set (used above as well). What is the intercept for fitting the model with x as the 
predictor and y as the outcome?

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

coef(lm(y~x)) 
#(Intercept)           x 
#1.567461   -1.712846 

plot(x,y,xlim=c(-2,2),ylim=c(-2,2))
abline(lm(y~x),col="red")
abline(h=0,col="gray")
abline(v=0,col="gray")

1.252
1.567 answer
2.105
-1.713

Question 8
You know that both the predictor and response have mean 0. What can be said about the intercept when you fit a 
linear regression?
x<-c(-4,-2,2,4)
mean(x)
## [1] 0
y<-c(-6,-1,3,4)
mean(y)
## [1] 0
coef(lm(y~x))
## (Intercept)           x 
##         0.0         1.2

It must be exactly one.
Nothing about the intercept can be said from the information given.
It must be identically 0. # answer
It is undefined as you have to divide by zero.

Question 9
Consider the data given by

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
What value minimizes the sum of the squared distances between these points and itself?

mu <- 0.573 
mu <- 0.8 
mu <- 0.36 
mu <- 0.44 
least_squares <- 0
for (i in 1:length(x)) {
    zz <- (x[i] - mu)^2 
    least_squares <- zz + least_squares
}
print (least_squares)

0.573 # answer
0.8
0.36
0.44

Question 10
Let the slope having fit Y as the outcome and X as the predictor be denoted as β1. Let the slope from fitting X as 
the outcome and Y as the predictor be denoted as γ1. Suppose that you divide β1 by γ1; in other words consider β1/γ1. 
What is this ratio always equal to?

x<-mtcars$wt
y<-mtcars$mpg
coef(lm(y~x))[2]/coef(lm(x~y))[2]
##     x 
## 37.94
var(y)/var(x)
## [1] 37.94

Var(Y)/Var(X) answer
2SD(Y)/SD(X)
1
Cor(Y,X)
