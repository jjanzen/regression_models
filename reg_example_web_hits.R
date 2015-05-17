
getwd()
setwd("/Users/a149174/JHDataScience/regression_models")
list.files()
data <- read.csv("~/JHDataScience/regression_models/web_hits_example/web_data2.csv", header=T)
head(data)
data$date <- as.Date(data$date,format = "%m/%d/%y")

plot(data$date, data$visits,pch=19,col="blue",main="Web Traffic", xlab="Date",ylab="Visits")
lm1 <- lm(data$visits ~ data$date)
abline(lm1,col="red",lwd=3)

summary(lm1)
lm1
#Coefficients:
#    (Intercept)    data$date  
#-2404.5259       148.9 

# each day, 149 more hits each day
-----------------------------
    # linear equation
    #NH = b0 +b1D + ei
    
    #NH - # hits
    #D - day of year
    #b0 - # hits on day 0
    #b1 - increase in # hits/day
    #ei - variation due to everything we didn't measure
    --------------------------
    # linear regression using logs to get increase per day
    round(exp(coef(lm(I(log(data$visits+1))~data$date))),4)
(Intercept)   data$date 
0.00000     1.00322 
# means 0.3% increase web hits per day
---------------------------
    # generalized linear model with poisson, consider using poisson to determine relative change over time
plot(data$date, data$visits,pch=19,col="green",,main="Web Traffic w/ Poisson",xlab="Date",ylab="Visits")
glm1 <- glm(data$visits ~ data$date, family="poisson")
abline(lm1,col="red",lwd=3) # for linear model line
lines(data$date,glm1$fitted,col="blue",lwd=3) # lm fit for possion
confint(glm1,level=0.95) # CI
#2.5 %        97.5 %
#(Intercept) -55.999943551 -45.190626728
#data$date     0.002976299   0.003632503
# to interpret, 95% confident the increase web hits/day falls between range of 0.003 and 0.004 or 0.3%/day



# time t would be equal one day, if every hour, t would equal 24
























-------------------from JH Reg lecture 03_03-----------------
getwd()
setwd("/Users/a149174/JHDataScience/regression_models")
dir.create("web_hits_example")
setwd("/Users/a149174/JHDataScience/regression_models/web_hits_example")
list.files()
download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="~/JHDataScience/regression_models/web_hits_example/gaData.rda",method="curl")
load("~/JHDataScience/regression_models/web_hits_example/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)

plot(gaData$date, gaData$visits,pch=19,col="darkgrey",xlab="Date",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$date)
abline(lm1,col="red",lwd=3)
lm1
#Coefficients:
 #   (Intercept)  gaData$date  
#-413.7595       0.0278  

-----------------------------
# linear equation
#NH = b0 +b1JD + ei

#NH - # hits
#JD - day of year (julian)
#b0 - # hits on day 0
#b1 - increase in # hits/day
#ei - variation due to everything we didn't measure
--------------------------
# linear regression using logs to get increase per day
round(exp(coef(lm(I(log(gaData$visits+1))~gaData$date))),5)
#(Intercept) gaData$date 
#0.00000     1.00231 
# means 0.2% increase web hits per day
---------------------------
# generalized linear model with poisson, consider using poisson to determine relative change over time
plot(gaData$date, gaData$visits,pch=19,col="green",xlab="Date",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$date, family="poisson")
abline(lm1,col="red",lwd=3) # for linear model line
lines(gaData$date,glm1$fitted,col="blue",lwd=3) # lm fit for possion
confint(glm1,level=0.95) # CI
#2.5 %        97.5 %
 #   (Intercept) -34.346577587 -31.159715656
#gaData$date   0.002190043   0.002396461
# to interpret, 95% confident the increase web hits/day falls between range of 0.0022 and 0.0024 or 0.2%/day



# time t would be equal one day, if every hour, t would equal 24








