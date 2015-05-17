“Is an automatic or manual transmission better for MPG”
"Quantify the MPG difference between automatic and manual transmissions"
Take the mtcars data set and write up an analysis to answer their question using regression models and 
exploratory data analyses.

Your report must be:
    
Written as a PDF printout of a compiled (using knitr) R markdown document.
Brief. Roughly the equivalent of 2 pages or less for the main text. Supporting figures in an appendix can 
be included up to 5 total pages including the 2 for the main report. The appendix can only include figures.
Include a first paragraph executive summary.

Did the student interpret the coefficients correctly?
Did the student do some exploratory data analyses?
Did the student fit multiple models and detail their strategy for model selection?
Did the student answer the questions of interest or detail why the question(s) is (are) not answerable?
Did the student do a residual plot and some diagnostics?
Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?

require(datasets)
data(mtcars)

# Exploratory
install.packages("GGally")
require(GGally); 
require(ggplot2)
g = ggpairs(mtcars, lower = list(continuous = "smooth"),params = c(method = "loess"))
g = ggpairs(mtcars,)
g = ggparis 
g

summary(mtcars)
head(mtcars)
?mtcars
[, 1]    mpg	Miles/(US) gallon
[, 2]	cyl	Number of cylinders
[, 3]	disp	Displacement (cu.in.)
[, 4]	hp	Gross horsepower
[, 5]	drat	Rear axle ratio
[, 6]	wt	Weight (lb/1000)
[, 7]	qsec	1/4 mile time
[, 8]	vs	V/S
[, 9]	am	Transmission (0 = automatic, 1 = manual)
[,10]	gear	Number of forward gears
[,11]	carb	Number of carburetors
# 1 - manual transmission
plot(mtcars$am,mtcars$mpg, pch=19, col="blue")
abline(lm(mtcars$mpg~mtcars$am),col="red")

# coefficients for entire dataset 
summary(lm(mtcars$mpg ~ ., data = mtcars))$coefficients
        
        Coefficients
            Estimate    Std. Error t value Pr(>|t|)  
(Intercept) 12.30337   18.71788   0.657   0.5181  
cyl         -0.11144    1.04502  -0.107   0.9161  
disp         0.01334    0.01786   0.747   0.4635  
hp          -0.02148    0.02177  -0.987   0.3350  
drat         0.78711    1.63537   0.481   0.6353  
wt          -3.71530    1.89441  -1.961   0.0633 .
qsec         0.82104    0.73084   1.123   0.2739  
vs           0.31776    2.10451   0.151   0.8814  
am           2.52023    2.05665   1.225   0.2340  
gear         0.65541    1.49326   0.439   0.6652  
carb        -0.19942    0.82875  -0.241   0.8122 
# model estimates based on slope coefficients.  I wanted to focus on variables which was a larger slope and relatively small Std. Error
# am:  2.52 mpg increase with a manual transmission
# wt:  3.71 mpg decrease per 1000 lb of weight increase.  Only issue hear is Std. Error is 
# qsec:  0.787 mpg increase for rear axle ratio

# correlations each variable to MPG
variables_mtcars <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
obj<-data.frame()
for (i in 1:length(mtcars)) {
    cor_mtcars <- cor(mtcars[,i],mtcars[,1])
    obj <- rbind(obj, cor_mtcars)
}
corr_variables_mpg <- cbind(variables_mtcars, obj)
colnames(corr_variables_mpg) <- c("variables","correlation_to_mpg")
corr_variables_mpg
variables correlation_to_mpg
1        mpg         1.0000000
2        cyl        -0.8521620
3       disp        -0.8475514
4         hp        -0.7761684
5       drat         0.6811719
6         wt        -0.8676594
7       qsec         0.4186840
8         vs         0.6640389
9         am         0.5998324
10      gear         0.4802848
11      carb        -0.5509251



# coefficients for mpg vs. am(tran)
summary(lm(mtcars$mpg ~ mtcars$am, data = mtcars))$coefficients
Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 17.147368   1.124603 15.247492 1.133983e-15
tran         7.244939   1.764422  4.106127 2.850207e-04
plot(mtcars$am,mtcars$mpg, pch=19, col="blue",xlab="AM(Transmission)",ylab="MPG")
abline(lm(mtcars$mpg~mtcars$am),col="red")

# coefficients for mpg vs. wt
summary(lm(mtcars$mpg ~ mtcars$wt, data = mtcars))$coefficients
Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 37.285126   1.877627 19.857575 8.241799e-19
mtcars$wt   -5.344472   0.559101 -9.559044 1.293959e-10
# plot mpg vs. wt  
plot(mtcars$wt,mtcars$mpg, pch=19, col="blue",xlab="Weight",ylab="MPG")
abline(lm(mtcars$mpg~mtcars$wt),col="red")
points(mtcars$wt,mtcars$mpg, pch=19, col=((mtcars$am=="1")*1+1))
legend("topright",c("automatic", "manual"),col=c("black","red"),pch=19)

# coefficients for mpg vs. cyl
summary(lm(mtcars$mpg ~ mtcars$cyl, data = mtcars))$coefficients
Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 37.88458  2.0738436 18.267808 8.369155e-18
mtcars$cyl  -2.87579  0.3224089 -8.919699 6.112687e-10
# plot mpg vs. cyl
plot(mtcars$cyl,mtcars$mpg, pch=19, col="blue",xlab="Cylinders(cyl)",ylab="MPG")
abline(lm(mtcars$mpg~mtcars$cyl),col="red")
points(mtcars$cyl,mtcars$mpg, pch=19, col=((mtcars$am=="1")*1+1))
legend("topright",c("automatic", "manual"),col=c("black","red"),pch=19)

# coefficients for mpg vs. qsec
summary(lm(mtcars$mpg ~ mtcars$qsec, data = mtcars))$coefficients
Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 37.285126   1.877627 19.857575 8.241799e-19
mtcars$wt   -5.344472   0.559101 -9.559044 1.293959e-10
# plot mpg vs. qsec  
plot(mtcars$qsec,mtcars$mpg, pch=19, col="blue",xlab="1/4 Time(qsec)",ylab="MPG")
abline(lm(mtcars$mpg~mtcars$qsec),col="red")
points(mtcars$qsec,mtcars$mpg, pch=19, col=((mtcars$am=="1")*1+1))
legend("topright",c("automatic", "manual"),col=c("black","red"),pch=19)

# coefficients for mpg vs. qsec for automatic transmission
summary(lm(mtcars_automatic$mpg ~ mtcars_automatic$qsec, data = mtcars_automatic))$coefficients
Estimate Std. Error   t value    Pr(>|t|)
(Intercept)           -9.009861  7.3094544 -1.232631 0.234488784
mtcars_automatic$qsec  1.438542  0.4002355  3.594238 0.002236703
# plot mpg vs. qsec  
plot(mtcars_automatic$qsec,mtcars_automatic$mpg, pch=19, col="blue",xlab="1/4 Time(qsec)",ylab="MPG")
abline(lm(mtcars_automatic$mpg~mtcars_automatic$qsec),col="red")
points(mtcars_automatic$qsec,mtcars_automatic$mpg, pch=19, col=((mtcars_automatic$am=="1")*1+1))
legend("topright",c("automatic", "manual"),col=c("black","red"),pch=19)


# coefficients for mpg vs. qsec for manual transmission
summary(lm(mtcars_manual$mpg ~ mtcars_manual$qsec, data = mtcars_manual))$coefficients
Estimate Std. Error   t value     Pr(>|t|)
(Intercept)        -23.520547 10.8043389 -2.176954 0.0521413550
mtcars_manual$qsec   2.759957  0.6193302  4.456358 0.0009684657
# plot mpg vs. qsec  
plot(mtcars_manual$qsec,mtcars_manual$mpg, pch=19, col="blue",xlab="1/4 Time(qsec)",ylab="MPG")
abline(lm(mtcars_manual$mpg~mtcars_manual$qsec),col="red")
points(mtcars_manual$qsec,mtcars_manual$mpg, pch=19, col=((mtcars_manual$am=="1")*1+1))
legend("topright",c("automatic", "manual"),col=c("black","red"),pch=19)

# subset mtcars for automatic transmission
mtcars_automatic <- subset(mtcars, mtcars[,9] == "0")

# subset mtcars for manual transmission
mtcars_manual <- subset(mtcars, mtcars[,9] == "1")

# suset mtcars for weight between 2.0 and 2.5 for comparison of qsec by transmission 
hist(mtcars_manual$wt, col = "red", main = "Weight", xlab = "Manual Transmisson")
hist(mtcars_automatic$wt, col = "red", main = "Weight", xlab = "Automatic Transmisson")

# using weight range of 2.5 - 3.5 will give 8 automatic samples and 5 manual samples
mtcars_weight <- subset(mtcars, mtcars[,6] < 2.5)
mtcars_weight <- subset(mtcars, mtcars[,6] > 3.5)

mtcars_automatic_weight <- subset(mtcars, mtcars_weight[,9] == "0")
mtcars_manual_weight <- subset(mtcars, mtcars_weight[,9] == "1")

# compare the MPG for 1/4 Mile times
quarter_mile_time <- c(14,16,18,20)
automatic_y_intercept <- summary(lm(mtcars_automatic_weight$mpg ~ mtcars_automatic_weight$qsec, data = mtcars_automatic_weight))$coefficients[1,1]
automatic_slope <- summary(lm(mtcars_automatic_weight$mpg ~ mtcars_automatic_weight$qsec, data = mtcars_automatic_weight))$coefficients[2,1]

mpg_quarter_auto <-data.frame()
for (i in 1:length(quarter_mile_time)) {
    mpg_qsec_automatic <- automatic_slope * quarter_mile_time[i] + automatic_y_intercept
    mpg_quarter_auto <- rbind(mpg_quarter_auto, mpg_qsec_automatic)
}
mpg_quarter_auto <- cbind("automatic",quarter_mile_time,mpg_quarter_auto)
colnames(mpg_quarter_auto) <- c("transmission","quarter time", "mpg")
mpg_quarter_auto

mpg_quarter_manual <-data.frame()
manual_y_intercept <- summary(lm(mtcars_manual_weight$mpg ~ mtcars_manual_weight$qsec, data = mtcars_manual_weight))$coefficients[1,1]
manual_slope <- summary(lm(mtcars_manual_weight$mpg ~ mtcars_manual_weight$qsec, data = mtcars_manual_weight))$coefficients[2,1]

for (i in 1:length(quarter_mile_time)) {
    mpg_qsec_manual <- manual_slope * quarter_mile_time[i] + manual_y_intercept
    mpg_quarter_manual <- rbind(mpg_quarter_manual, mpg_qsec_manual)
}
mpg_quarter_manual <- cbind("manual",quarter_mile_time,mpg_quarter_manual)
colnames(mpg_quarter_manual) <- c("transmission","quarter time", "mpg")
mpg_quarter_manual

mpg_delta <- data.frame()
for (i in 1:length(quarter_mile_time)) {
    diff <- mpg_quarter_auto[i,3] - mpg_quarter_manual[i,3]
    mpg_delta <- rbind(mpg_delta, diff)
}
mpg_delta <- cbind(quarter_mile_time,mpg_delta)
colnames(mpg_delta) <- c("quarter_mile_time","mpg(auto - manual)")
mpg_delta

# not used

# testing to add ggplot (samle from swiss dataset from coursera)
library(dplyr); 
swiss = mutate(mtcars, mpgBin = 1 * (mpg < 20))
swiss
g = ggplot(swiss, aes(x = am, y = mpg, colour = factor(mpgBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("Transmission") + ylab("MPG")
g

#http://www.inside-r.org/packages/cran/GGally/docs/ggpairs
# Custom Examples
custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
custom_car
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
custom_car <- putPlot(custom_car, plot, 1, 2)
personal_plot <- ggally_text(
    "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
)
custom_car <- putPlot(custom_car, personal_plot, 1, 3)
custom_car
