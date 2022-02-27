## Module 4 Deep Analytics and Visualization Task 2: 
# Visualize and Analyze Energy Data. (Ampliation of Task 1)

#install.packages("RMySQL")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("plotly")
#install.packages("forecast")

# Task1
library(RMySQL)
library(dplyr)
library(lubridate)

# Task2
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

## Create a database connection 
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!',
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con) # List of the 6 table inside the database.

# We will use iris as an example.

## Lists attributes contained in a table:
dbListFields(con,'iris') # Show 6 attributes

# Still focusing on "iris", we can query the database. We can download all of 
# the data or choose the specific attributes we’re interested in. 

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")


#######################

# We start taking all tables from yr_2006 to yr_2010

yr_2006ALL <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007ALL <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008ALL <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009ALL <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010ALL <- dbGetQuery(con, "SELECT * FROM yr_2010")



# Checking 2006
str(yr_2006ALL)
summary(yr_2006ALL)
head(yr_2006ALL)
tail(yr_2006ALL)

# We just have 21.992 observations. It starts from 2006-12-16. Just half month.




# Checking 2007
str(yr_2007ALL)
summary(yr_2007ALL)
head(yr_2007ALL)
tail(yr_2007ALL)

# Full year.



# Checking 2008
str(yr_2008ALL)
summary(yr_2008ALL)
head(yr_2008ALL)
tail(yr_2008ALL)

# Full year.




# Checking 2009
str(yr_2009ALL)
summary(yr_2009ALL)
head(yr_2009ALL)
tail(yr_2009ALL)

# Full year.



# Checking 2010
str(yr_2010ALL)
summary(yr_2010ALL)
head(yr_2010ALL)
tail(yr_2010ALL)

# Interrupted at 2010-11-26.

## So, Full years: 2007, 2008, 2009. We will combine all this information
## in a single database. Just the full years.

DF<-bind_rows(yr_2007ALL, yr_2008ALL, yr_2009ALL) # Combine dataframes


# Checking DF
str(DF)
summary(DF)
head(DF)
tail(DF)

# They are in correct order.

## Now we will combine Date and Time to convert them to the correct format

## Combine Date and Time attribute values in a new attribute column
DF<-cbind(DF,paste(DF$Date,DF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 11th column a header name 

colnames(DF)[11] <-"DateTime"

## Move the DateTime attribute within the dataset. 
DF <- DF[,c(ncol(DF), 1:(ncol(DF)-1))]
head(DF) # Now the last column DateTime is the first one.

#You will now want to convert the new DateTime attribute to a DateTime data type
# called POSIXct. After converting to POSIXct we will add the time zone to 
# prevent warning messages. The data description suggests that the data is from 
# France.

## Convert DateTime from character to POSIXct 
DF$DateTime <- as.POSIXct(DF$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(DF$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(DF)


# Extract "Year" information from DateTime using the Lubridate "year" function 
# and create an attribute for year

## Create "year" attribute with lubridate

DF$year <- year(DF$DateTime)

# Now there is a new column with the year. 
# Like "year", Libridate also has functions to create attributes for quarter, 
# month, week, weekday, day, hour and minute.

DF$month <- month(DF$DateTime)
DF$day <- day(DF$DateTime)
DF$minute <- minute(DF$DateTime)
DF$hour <- hour(DF$DateTime)
DF$week <- week(DF$DateTime)
DF$weekDay <- wday(DF$DateTime)

# Remark: apparently, the hour denotes the hour where it belongs. So, the first
# 59 minutes belong the "hour 1".

summary(DF)

## A priori analysis of the descriptives:

# Global active mean: 1.089 kilowatt.
# Global reactive mean: 0.1219 kilowatt
# Global intensity mean: 4.624 amperes.
# Voltage mean: 240.6 volts.
# sub metering 1 (kitchen): 1.159 watt/hour.
# sub metering 2 (laundry room): 1.343 watt/hour.
# sub metering 3 (water-heater and air-conditioner): 6.216 watt/hour.

# in the sub meterings all 3 minimums are 0, but the maximums are 82, 78 and 31,
# respectively. It is interesting that sub metering 3 is clearly the one using 
# more power in mean (more than a x4 wrt sub metering 2), but the maximum is 31 
# is much lower than maximums in sub metering 1 and 2. By the descriptives, we
# may expect pretty "non-smooth" distributions in all three cases, but 
# specifically in 1 and 2. In those cases we can expect high pikes, 
# since in those cases we are dealing with  heavy on demand power-using tools 
# like ovens , washing machines, etc. 

# However, it is somehow suspicious that sub metering 2 has a median of 0. 
# Supposedly, refrigerator should be on sub metering 2, and it is a pretty 
# consuming appliance that should always be on. 

# We may confirm some insights through some extra EDA. We'll do it on a random 
# week.

df<- filter(DF, Date >= "2007-01-01" & Date <= "2007-01-07") # Take a week

# 1st plot: global active power/date
plot(df$DateTime, df$Global_active_power, type = "l", main = NULL, xlab = "time", ylab = "Global Active Power")

# 2nd plot voltage/date
plot(df$DateTime, df$Voltage, type = "l", main = NULL, xlab = "time", ylab = "Voltage")

# 3rd plot Energy Sub meterings/date
plot(df$DateTime, df$Sub_metering_1, type = "l", main = NULL, xlab = "time", ylab = "Energy sub metering ")
lines(df$DateTime, df$Sub_metering_2, type = "l", col = "red", main = NULL, xlab = "time", ylab = "")
lines(df$DateTime, df$Sub_metering_3, type = "l", col = "blue", main = NULL, xlab = "time", ylab = "")
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black", "red", "blue"), lty=1, cex=0.8, box.lty=0, inset = 0.02)




###### Start of M4T2

## Plot all of sub-meter 1. 

# plot(DF$Sub_metering_1)

## We cannot extract proper information. It is better to 
## reduce the numbers of points (granularity).

## Subset the second week of 2008 - All Observations
houseWeek <- filter(DF,year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)



## Subset the 9th day of January 2008 - All observations
houseDay <- filter(DF, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')




## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(DF, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))




## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



### Week visualization 30 min granularity




## Subset the 9th day of January 2008 - 10 Minute frequency
houseWeek2 <- filter(DF, year == 2008 & month == 1 & week == 2 & (minute == 20 | minute == 50))




## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Week 1/2 hour Gran., 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))





### Same Week visualization 60 min granularity. 




## Subset the 9th day of January 2008 - 10 Minute frequency
houseWeek2 <- filter(DF, year == 2008 & month == 1 & week == 2 & minute == 20)




## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Week 1h Gran., 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))












### Same week (2009) visualization 30 min granularity




## Subset the 9th day of January 2008 - 10 Minute frequency
houseWeek2 <- filter(DF, year == 2009 & month == 1 & week == 2 & (minute == 20 | minute == 50))




## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption  Second Week 1/2 hour Gran., 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))





### Same Week visualization 60 min granularity. 




## Subset the 9th day of January 2008 - 10 Minute frequency
houseWeek2 <- filter(DF, year == 2009 & month == 1 & week == 2 & minute == 20)




## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Week 1h Gran., 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




### Prepare to analyze

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(DF, weekDay == 2 & hour == 20 & minute == 1)



## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages):

# library(ggplot2)
# library(ggfortify)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)



### Represent all three submeterings in the same period.


## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809weekly, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")



## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809weekly, ts.colour = 'orange', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")



## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")



### Forecasting a time series.

## Apply time series linear regression to the sub-meter 3 ts object and use 
## summary to obtain R2 and RMSE from the model you built
# library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)


## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)


# Lets do one more plot. This time we will add labels, change the confidence 
# levels and plot only the forecast portion that is above zero. 

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")




### Now we will create all three forecast for each submetering:



##Forecast Submetering 1:
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)
## RMSE: 4.13
## Multiple R2: 0.3437
## Adjusted R2:  0.01556


## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")



##Forecast Submetering 2:
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)

## RMSE: 6.323
## Multiple R2: 0.3343
## Adjusted R2:0.001384 

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")



##Forecast Submetering 3:
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## RMSE: 6.871
## Multiple R2: 0.3831
## Adjusted R2: 0.07461 

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")




### Decomposing a Seasonal Time Series:

## In order to correctly estimate any trend and seasonal components that might be in your time series you need to use the decompose() function in the forecast package, which estimates the trend, seasonal, and irregular components of a time series.

#When you use the decompose() function, R returns three different objects that can be accessed from the command line after running decompose() on your time series. For example: If your time series is called ‘timeseriesOne’ the following R objects can be accessed:
  
#Seasonal component: timeseriesOne$seasonal
#Trend component: timeseriesOne$trend
#Random component: timeseriesOne$random

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)



# All 3 Decompositions for each sub-metering

## Sub-metering 1

## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly)




## Sub-metering 2

## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly)



## Sub-metering 3

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)



### Holt-Winters Forecasting

##To make forecasts using simple exponential smoothing, you can fit a simple 
#exponential smoothing predictive model using the HoltWinters() function from 
#the stats package for R. 

##Remove Seasonal Components:
##To use HoltWinters() and forecasting you will first need to remove the 
##seasonal component that you identified via decomposition. You can use seasonal
##adjusting for this as seen in the Little Book of R for Time Series. 
##Let's continue to use sub-meter 3 and its decomposed components.

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## You really can't tell if seasonality has been removed by looking at the plot.
##  Let's try decompose again and see if the  seasonal component was removed. 

## Test Seasonal Adjustment by running Decompose again. Note the very, 
## very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

##Yes there is a seasonal line, but look at the scale for the seasonal section.
##-1e-15 through 5e-16. That's a decimal with 15 zeros before 1. A very very 
##small number indeed. For all practical purposes the seasonality has been 
##removed. 

## HoltWinters Simple Exponential Smoothing
##Now that we have removed the seasonal component let's use HoltWinters 
##simple exponential smoothing.

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))


##In the plot above the exponentially smooth fitted line is plotted in red 
##along with the original data points. How does exponential smoothing help? 
##Consider the outliers. Consider the information we removed when we subsetted 
##millions of data points to 52 observations per year. 


## HoltWinters Forecast
#Having created a ts object that contains exponentially smoothed data with no 
#seasonality, let’s now use forecast again. 

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


## Lastly, let's change the the confidence levels and then plot only the 
##forecasted area. Think of this just as you would when a weatherperson 
##forecasts the weather: They don't include the preceding years, weeks and days. 

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

##The resulting image shows a very consistent forecast for sub-meter 3. 
##This might not be true for the other sub-meters. Ask yourself, how is this
##plot different from the forecasted plot from step three of the plan of attack?
##Which, if any, is more useful? 



### 3 New forecast:


### Forecast Sub-mettering 1
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)

plot(decompose(tsSM1_070809Adjusted))

tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))

tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))

plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))





### Forecast Sub-mettering 2
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)

plot(decompose(tsSM2_070809Adjusted))

tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))

tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))

plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))


### Forecast Sub-mettering 3


tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

plot(decompose(tsSM3_070809Adjusted))

tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))

plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))



