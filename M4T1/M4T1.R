## Module 4 Deep Analytics and Visualization Task 1: Domain research and
# Exploratory data analysis.

#install.packages("RMySQL")
#install.packages("dplyr")
#install.packages("lubridate")

library(RMySQL)
library(dplyr)
library(lubridate)

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


