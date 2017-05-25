# globals
dataUrl = paste("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip")

# read data set from internets
# Read the Energy Consumption Data
temp <- tempfile()
download.file(dataUrl, temp)
PowerConsumption <- read.table(
  unz(
    temp,
    "household_power_consumption.txt"
  ),
  sep=";",
  header=TRUE
)
##Read the Energy Consumption Data
#PowerConsumption<- read.table("~/Desktop/household_power_consumption.txt", 
#                                    sep=";" , header = TRUE)

#Create one date+ time column called DateTime
PowerConsumption <-cbind(PowerConsumption,paste(PowerConsumption$Date,PowerConsumption$Time), 
            stringsAsFactors=FALSE)
colnames(PowerConsumption)[10] <-"DateTime"
PowerConsumption <- PowerConsumption[,c(ncol(PowerConsumption), 1:(ncol(PowerConsumption)-1))]
head(PowerConsumption)

#Convert date and Time from chr to POSIXlt object
PowerConsumption$DateTime <- strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S")
PowerConsumption$Date <- as.Date(PowerConsumption$Date, "%d/%m/%Y")
str(PowerConsumption)

# remove the old date and time cols
#PowerConsumption <- PowerConsumption[,-c(2:3)]
#PowerConsumption <- PowerConsumption[,-c(3)]

# create year / month / day features
PowerConsumption$year <- as.factor(strftime(PowerConsumption[,1], format="%Y"))
PowerConsumption$month <- as.factor(strftime(PowerConsumption[,1], format="%b"))
PowerConsumption$dayOfMonth <- as.factor(strftime(PowerConsumption[,1], format="%e"))
PowerConsumption$hour <- as.factor(strftime(PowerConsumption[,1], format="%k"))

#Convert data to numeric
PowerConsumption$Voltage<-as.numeric(PowerConsumption$Voltage)
PowerConsumption$Global_active_power<-as.numeric(PowerConsumption$Global_active_power)
PowerConsumption$Global_reactive_power<-as.numeric(PowerConsumption$Global_reactive_power)
PowerConsumption$Global_intensity<-as.numeric(PowerConsumption$Global_intensity)
PowerConsumption$Sub_metering_1<-as.numeric(PowerConsumption$Sub_metering_1)
PowerConsumption$Sub_metering_2<-as.numeric(PowerConsumption$Sub_metering_2)
library(dplyr)

Powerdata <- PowerConsumption[,4:14] %>%
  group_by(year, month, dayOfMonth, hour) %>%
  summarise(SM1=sum(Sub_metering_1, na.rm=TRUE),
            SM2=sum(Sub_metering_3, na.rm=TRUE),
            SM3=sum(Sub_metering_2, na.rm=TRUE))
           
          
#TS<-ts(Powerdata[1:24,5:7], frequency=24, start=0, end=23)
#TS
#TS<-ts(Powerdata, frequency=12, start=c(2001,1), end=c(2008,12))
plot(TS, xlab="Hour", ylab="Consumption", main="daily data", type="l")
plot(TS)

par(mfrow=c(2,1))
require(PerformanceAnalytics)
PerformanceAnalytics::chart.TimeSeries(as.xts(TS))
plot(TS, plot.type="s")


#Create HoltWinters
hw<-HoltWinters(TS, beta = FALSE, gamma = FALSE)  
hw
hw$fitted
plot(hw)
library(forecast)
seasonplot(TS)

##Decompose 
##How to make it univariate?
TScomponent<-decompose(TS)
TScomponent$seasonal
TScomponent$trend
TScomponent$random
plot(TScomponent)

library("TTR")
TS_SMA<-SMA(TS,n=3)

accuracy(hw)
forecast(hw,3)

fit<-stl(TS, s.window ="period")
#plot(fit)

#require(xts)
#plot(as.xts(TS), major.format = "%Y-%b")

#quarter<-(cycle(TS) -1) %/% 3
#quarter
#plot(quarter)
#monthplot(TS)
#monthplot(TS, phase = quarter)

#install.packages("ggplot2")
#library(ggplot2)
#ggplot(TS, aes(x=Month, y=Consumption)) +geom_line()

#ConsumptionTS <- ts(newdata$Sub_metering_2, frequency = 100, start = c(2006,1))
#plot(ConsumptionTS)
#dt<-dplyr::filter(newdata, Date == "2006-12-16")

#lines(HoltWinters(ConsumptionTS)$fitted, col="red")

#Powerdata$Dt<-paste0(Powerdata$year,Powerdata$month,Powerdata$dayOfMonth) 
#Powerdata$Dt<-paste0(Powerdata$year,Powerdata$month)
#PowerConsumption$DateTime<-NULL

####Missing values
#newdata<-na.omit(PowerConsumption)
#table(is.na(PowerConsumption))

#dplyr::summarise(PowerConsumption, avg=mean(Global_intensity))
#dplyr::summarise(PowerConsumption, avg=mean(Voltage))
#dplyr::summarise(PowerConsumption, avg=mean(Global_active_power))
#dplyr::summarise(PowerConsumption, avg=mean(Global_reactive_power))
#dplyr::summarise(PowerConsumption, avg=mean(Sub_metering_1))
#dplyr::summarise(PowerConsumption, avg=mean(Sub_metering_2))
#dplyr::summarise(PowerConsumption, avg=mean(Sub_metering_3))