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
  header=TRUE,
  as.is=TRUE
)
##Read the Energy Consumption Data
#PowerConsumption<- read.table("~/Desktop/household_power_consumption.txt", 
#                                    sep=";" , header = TRUE)

#Create one date+ time column called DateTime
PowerConsumption <-cbind(PowerConsumption,paste(PowerConsumption$Date,PowerConsumption$Time), 
            stringsAsFactors=FALSE)
colnames(PowerConsumption)[10] <-"DateTime"

#move DateTime column to col 1 of the table
PowerConsumption <- PowerConsumption[,c(ncol(PowerConsumption), 1:(ncol(PowerConsumption)-1))]
head(PowerConsumption)

# remove the old date and time cols
PowerConsumption <- PowerConsumption[,-c(2:3)]

#Convert date and Time from chr to POSIXlt object
#PowerConsumption$DateTime <- as.POSIXct(strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S"))
PowerConsumption$DateTime <- strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S")
str(PowerConsumption)

#Convert data to numeric
PowerConsumption$Voltage<-as.numeric(as.character(PowerConsumption$Voltage))
PowerConsumption$Global_active_power<-as.numeric(as.character(PowerConsumption$Global_active_power))
PowerConsumption$Global_reactive_power<-as.numeric(as.character(PowerConsumption$Global_reactive_power))
PowerConsumption$Global_intensity<-as.numeric(as.character(PowerConsumption$Global_intensity))
PowerConsumption$Sub_metering_1<-as.numeric(as.character(PowerConsumption$Sub_metering_1))
PowerConsumption$Sub_metering_2<-as.numeric(as.character(PowerConsumption$Sub_metering_2))

PowerConsumption[1:10,c("DateTime", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]          
#require(xts)
#xts2<-xts::as.xts(PowerConsumption[1:10000,1])
#xts::first(xts2,'1 week')

# create year / month / day features
PowerConsumption$year <- as.factor(strftime(PowerConsumption[,1], format="%Y"))
PowerConsumption$month <- as.factor(strftime(PowerConsumption[,1], format="%b"))
PowerConsumption$dayOfMonth <- as.factor(strftime(PowerConsumption[,1], format="%e"))
PowerConsumption$hour <- as.factor(strftime(PowerConsumption[,1], format="%k"))

#PowerConsumption$Date<-as.factor(strftime(PowerConsumption[,1], format="%Y-%m-%e"))
#Powerdata_Date<-PowerConsumption[,2:13] 
#Powerdata_Date <- Powerdata_Date[,c(ncol(Powerdata_Date), 1:(ncol(Powerdata_Date)-1))]
#Powerdata_Date<-Powerdata_Date[,1:8]

library(dplyr)
##Day One data
Powerdata_day <- PowerConsumption[,2:12] %>%
  group_by(year, month, dayOfMonth, hour) %>%
  summarise(SM1=sum(Sub_metering_1, na.rm=TRUE),
            SM2=sum(Sub_metering_3, na.rm=TRUE),
            SM3=sum(Sub_metering_2, na.rm=TRUE))
TS_day<-ts(Powerdata_day[1:24,5:7], frequency=24, start=0, end=23)
plot(TS_day, xlab="Hour", ylab="Consumption", main="Day One data", type="l")

## Monthly Aggregate data
Powerdata_month <- PowerConsumption[,2:12] %>%
  group_by(month) %>%
  summarise(SM1=sum(Sub_metering_1, na.rm=TRUE),
            SM2=sum(Sub_metering_3, na.rm=TRUE),
            SM3=sum(Sub_metering_2, na.rm=TRUE))
TS_month<-ts(Powerdata_month[,2:4], frequency=12)
head(TS_month)
#TS_month<-ts(Powerdata_month[,2:4])
plot(TS_month) # 3 charts- one each for SM1, SM2 and SM3
require(xts)
plot(as.xts(TS_month),major.format="%B", main="Monthly Data") #only 1 chart once I introduced month label

##Yearly data
Powerdata_year <- PowerConsumption[,2:13] %>%
  group_by(month, year) %>%
  summarise(SM1=sum(Sub_metering_1, na.rm=TRUE),
            SM2=sum(Sub_metering_3, na.rm=TRUE),
            SM3=sum(Sub_metering_2, na.rm=TRUE))

##Quarterly data
Powerdata_2008_Q1<-subset(Powerdata_year, year=="2008" & (month=="Jan" | month=="Feb" | month=="Mar"))
TS_2008_Q1<-ts(Powerdata_2008_Q1[,3:5])
plot(TS_2008_Q1)

#par(mfrow=c(2,1))
#at <- paste0("200", c("8-01","8-02","8-03"))
#plot(TS_2008_Q1, plot.type="s", format="%b\n%Y", at=at)
#require(PerformanceAnalytics)
#require(xts)
#plot(as.xts(TS_2008_Q1))
#xts::plot.xts(as.xts(TS_2008_Q1))
#PerformanceAnalytics::chart.TimeSeries(as.xts(TS_2008_Q1))
#Month<-c("Jan","Feb","March")
#plot(Month,TS_2008_Q1)

Powerdata_2008_Q2<-subset(Powerdata_year, year=="2008" & (month=="Apr" | month=="May" | month=="Jun"))
TS_2008_Q2<-ts(Powerdata_2008_Q2[,3:5])
plot(TS_2008_Q2)

Powerdata_2008_Q3<-subset(Powerdata_year, year=="2008" & (month=="Jul" | month=="Aug" | month=="Sep"))
TS_2008_Q3<-ts(Powerdata_2008_Q3[,3:5])
plot(TS_2008_Q3)

Powerdata_2008_Q4<-subset(Powerdata_year, year=="2008" & (month=="Oct" | month=="Nov" | month=="Dec"))
TS_2008_Q4<-ts(Powerdata_2008_Q4[,3:5])
plot(TS_2008_Q4)

require(PerformanceAnalytics)
par(mfcol=c(2,2))
ts.plot(TS_2008_Q1, plot.type="s", main="Q1 data", gpars= list(col=rainbow(10)))
ts.plot(TS_2008_Q2, plot.type="s", main="Q2 data", gpars= list(col=rainbow(10)))
ts.plot(TS_2008_Q3, plot.type="s", main="Q3 data", gpars= list(col=rainbow(10)))
ts.plot(TS_2008_Q4, plot.type="s", main="Q4 data", gpars= list(col=rainbow(10)))

#grid<-matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE)
#grid
#layout(grid)
#plot(TS_2008_Q1)
#plot(TS_2008_Q2)
#plot(TS_2008_Q3)

#TS_year<-ts(Powerdata_year[,3:5], start=c(2006), end=c(2012), frequency = 4)
TS_year<-ts(Powerdata_year[,3:5], start=c(2008,1), end=c(2009,12), frequency = 12)
#TS_year<-ts(Powerdata_year[,3:5], start=c(2008))
head(TS_year)
plot(TS_year, col=rainbow(10)) # 3 charts

dates <- seq(as.Date("01/01/2008", format = "%d/%m/%Y"),
             by="months", length=length(TS_year))
plot(dates,TS_year, xaxt="n", ann=FALSE, type="l", col=rainbow(10))
labDates <- seq(as.Date("01/12/2006", format = "%d/%m/%Y"), tail(dates, 1),
                by = "months")
axis.Date(side = 1, dates, at = labDates, format = "%b %y", las = 2) # only 1 chart

#library(ggplot2)
#library(scales)
#p = ggplot(data = Powerdata_Date, aes(x = Date)) + geom_line()
#p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) 
 # theme(axis.text.x = element_text(angle = 45))
  
#PerformanceAnalytics::chart.TimeSeries(as.xts(TS))


#Create HoltWinters
holt<-HoltWinters(TS_year[,3], alpha=0.1, beta = 1, gamma = 1)  
plot(holt)

library(forecast)
Power_forecast <- predict(holt, n.ahead = 2)
plot.ts(Power_forecast)

seasonplot(TS_year)
plot(holt,Power_forecast)

##Decompose 
#TS_year<-ts(Powerdata_year[,3:5], start=c(2008,1), end=c(2009,12), frequency = 12)
TScomponent<-decompose(TS_year)
season<-TScomponent$seasonal
trend<-TScomponent$trend
rand<-TScomponent$random
plot(TScomponent)
plot(season)
plot(trend)
plot(rand)
ts.plot(cbind(trend,trend*season),lty=1:2)














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