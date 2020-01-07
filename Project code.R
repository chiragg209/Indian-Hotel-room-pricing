# Hotel Room Pricing In The Indian Market
# NAME: Chirag Gupta
# EMAIL: chirag.gupta364@gmail.com
# COLLEGE : IIT (ISM) Dhanbad 

# Loading the Data set
hoteldf <-read.csv(paste("Cities42.csv",sep = ""))

# General view of the entire Dataframe 
View(hoteldf)
# Details of datatypes of each variable
str(hoteldf)

# Summarizing the data to understand the statistics of  each variable
summary(hoteldf)

### DATA CLEANING ###

# "hoteldf" contains many variables but there  are some discrepancies in dataset...
# ...and some variables are unappropriate and absurd which should be removed.


# identifying discrepancies in POPULATION column of dataset
dim(table(hoteldf$CityName))   # output =42
dim(table(hoteldf$Population)) # output =44
# as output are not equal it means there is discrepancy in Population
table(hoteldf$Population)
# discrepancy found in city "Munnar" population

# Removing discrepancies in POPULATION column of dataset
hoteldf$Population[hoteldf$Population==38472 | hoteldf$Population==38473]<-38471

dim(table(hoteldf$Population)) # output =42
table(hoteldf$Population)


# identifying discrepancies in DATE column of dataset
dim(table(hoteldf$Date))   # output =20 due to different formats (which should be 8)
table(hoteldf$Date)

# removing discrepancies in DATE column of dataset
hoteldf$Date <- as.character(hoteldf$Date)
hoteldf$Date[hoteldf$Date=="18-Dec-16"] <-"Dec 18 2016"
hoteldf$Date[hoteldf$Date=="21-Dec-16"] <-"Dec 21 2016"
hoteldf$Date[hoteldf$Date=="24-Dec-16"] <-"Dec 24 2016"
hoteldf$Date[hoteldf$Date=="25-Dec-16"] <-"Dec 25 2016"
hoteldf$Date[hoteldf$Date=="28-Dec-16"] <-"Dec 28 2016"
hoteldf$Date[hoteldf$Date=="31-Dec-16"] <-"Dec 31 2016"
hoteldf$Date[hoteldf$Date=="4-Jan-16" | hoteldf$Date=="4-Jan-17" | hoteldf$Date=="Jan 4 2017"] <-"Jan 04 2017"
hoteldf$Date[hoteldf$Date=="8-Jan-16" | hoteldf$Date=="8-Jan-17" | hoteldf$Date=="Jan 8 2017"] <-"Jan 08 2017"

dim(table(hoteldf$Date))   # output =8
table(hoteldf$Date)
# Dates are converted into numeric values using dummy variables 1 to 8
hoteldf$Date <- as.numeric(as.factor(hoteldf$Date))

# converting HotelNames to HotelRank using dummy variables 1 to 1670
hoteldf$HotelRank <- as.numeric(as.factor(hoteldf$HotelName))

# removing absurd and irrelevant variables
hoteldata <- hoteldf[,-c(1,2,10,14,15,16)]
View(hoteldata)

# Summarizing the new cleaned dataset to understand the statistics of  each variable
summary(hoteldata)
library(psych)
describe(hoteldata)

# selecting most important 3 variables for predicting Hotel Rent using Boruta Package
library(Boruta)
impout <-Boruta(RoomRent~ Population + CityRank +IsMetroCity + IsTouristDestination + IsWeekend +
                StarRating +Airport +  FreeWifi + IsNewYearEve + Date + HotelCapacity +
               HasSwimmingPool + FreeBreakfast + HotelRank , data= hoteldata)
print (impout)
plot(impout, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(impout$ImpHistory) ,function(i) impout$ImpHistory[is.finite(impout$ImpHistory[,i]),i])
names(lz) <- colnames(impout$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(impout$ImpHistory), cex.axis = 0.7)


## StarRating , Airport , HotelCapacity are 3 most important variables
## RoomRent = F(StarRating , Airport , HotelCapacity)
library(ggplot2)
table(hoteldata$StarRating)
ggplot(hoteldata, aes(x=StarRating))+ geom_bar()
library(lattice)
bwplot(hoteldata$StarRating, horizontal = TRUE, xlab = "Rating (out of 5)",main = "Rating of hotels of different cities")
bwplot(hoteldata$Airport, horizontal = TRUE, xlab = "Distance (in Kms)", main = "Distance between Hotel and closest major Airport")
bwplot(hoteldata$HotelCapacity, horizontal = TRUE, main = "Capacity of different Hotels")

library(car)
#Visualizing relation between StarRating and RoomRent
scatterplot(hoteldata$StarRating,hoteldata$RoomRent,ylim=c(0,50000),main="RoomRent Vs StarRating", xlab="Star rating" ,ylab = "RoomRent(INR)")

#Visualizing relation between RoomRent and Airport
scatterplot(hoteldata$Airport,hoteldata$RoomRent,ylim=c(0,50000),main="RoomRent Vs Airport", ylab="RoomRent(INR)",xlab = "Distance of airport (in Kms)")

#Visualizing relation between RoomRent and HotelCapacity
scatterplot(hoteldata$HotelCapacity,hoteldata$RoomRent,ylim=c(0,50000),main="RoomRent Vs HotelCapacity", ylab="RoomRent(INR)",xlab = "Hotel Capacity")


# understanding how are the most important variables correlated pair-wise
library(corrgram)
corrgram(hoteldata[,c("StarRating","Airport","HotelCapacity","RoomRent")], lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,main="Corrgram of Hotel  data")

# Variance-Covariance Matrix 
cov(hoteldata[,c("StarRating","Airport","HotelCapacity","RoomRent")])


#1. H1 : The Hotels which having swimmingpools have higher RoomRent.
aggregate(hoteldata$RoomRent, list(swimmingpool=hoteldata$HasSwimmingPool),mean)
boxplot( RoomRent~HasSwimmingPool, hoteldata, horizontal=TRUE,main="RoomRent Vs SwimmingPool",ylab="HasSwimmingPool" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ HasSwimmingPool , data=hoteldata, alternative ="less")

#2. H1 : The hotels in city having tourist destination have higher RoomRent.
aggregate(hoteldata$RoomRent, list(TouristDestination=hoteldata$IsTouristDestination),mean)
boxplot(RoomRent~IsTouristDestination, hoteldata, horizontal=TRUE,main="RoomRent Vs TouristDestination",ylab="IsTouristDestination" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ IsTouristDestination , data=hoteldata, alternative ="less")

#3. H1 : The Hotels having free breakfast have higher RoomRent.
aggregate(hoteldata$RoomRent, list(FreeBreakfast=hoteldata$FreeBreakfast),mean)
boxplot(RoomRent~FreeBreakfast, hoteldata, horizontal=TRUE,main="RoomRent Vs FreeBreakfast",ylab="FreeBreakfast" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ FreeBreakfast , data=hoteldata, alternative ="less")

#4. H1 : The Hotels in non Metro city have higher RoomRent than metro city.
aggregate(hoteldata$RoomRent, list(Metrocity=hoteldata$IsMetroCity),mean)
boxplot(RoomRent~IsMetroCity, hoteldata, horizontal=TRUE,main="RoomRent Vs IsMetroCity",ylab="IsMetroCity" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ IsMetroCity , data=hoteldata, alternative ="greater")

#5. H1 : The Hotels  having free wifi have higher RoomRent.
aggregate(hoteldata$RoomRent, list(Freewifi=hoteldata$FreeWifi),mean)
boxplot(RoomRent~FreeWifi, hoteldata, horizontal=TRUE,main="RoomRent Vs FreeWifi",ylab="FreeWifi" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ FreeWifi , data=hoteldata, alternative ="less")

#6. H1 : The Hotels on newyeareve have higher RoomRent.
aggregate(hoteldata$RoomRent, list(NewyearEve=hoteldata$IsNewYearEve),mean)
boxplot(RoomRent~IsNewYearEve, hoteldata, horizontal=TRUE,main="RoomRent Vs NewYearEve",ylab="IsNewYearEve" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ IsNewYearEve , data=hoteldata, alternative ="less")

#7. H1 : The Hotels on weekend have higher RoomRent.
aggregate(hoteldata$RoomRent, list(Weekend=hoteldata$IsWeekend),mean)
boxplot(RoomRent~IsWeekend, hoteldata, horizontal=TRUE,main="RoomRent Vs Weekend",ylab="IsWeekend" ,xlab = "RoomRent(INR)")
t.test(RoomRent ~ IsWeekend, data=hoteldata, alternative ="less")


corrgram(hoteldata, main = "corrgram for factors vs Hotel Room Prices", lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt)

# Analysing correlation of RoomRent with different factors
scatterplotMatrix(formula = ~  CityRank + HotelRank + HasSwimmingPool + Population + RoomRent,
                  cex=0.6, data=hoteldata, diagonal="histogram")

scatterplotMatrix(formula = ~  IsMetroCity + IsTouristDestination + IsWeekend + IsNewYearEve + RoomRent,
                  cex=0.6, data=hoteldata, diagonal="histogram")

# Formulating multivariate linear regression model to fit room rent with respect to different factors

# model 1 - with only most important features
fit1<-lm(RoomRent ~ StarRating + Airport +HotelCapacity + HasSwimmingPool, data=hoteldata)
summary(fit1)
AIC(fit1)

# model 2 - with all features in "hoteldata"
fit2<-lm(RoomRent ~ . , data=hoteldata)
summary(fit2)
AIC(fit2)

# model 3 - best fit model
fit3 <- lm(RoomRent ~ . - CityRank - FreeBreakfast - IsWeekend, data=hoteldata)
summary(fit3)
# model 3 is equivalent to (RoomRent ~ StarRating + HotelRank + Airport + HotelCapacity + HasSwimmingPool + Population+
#             IsMetroCity + IsTouristDestination  + Date+ FreeWifi  + IsNewYearEve ,data = hoteldata)

# AIC of best model
AIC(fit3)
#Coefficents of the best model
fit3$coefficients

