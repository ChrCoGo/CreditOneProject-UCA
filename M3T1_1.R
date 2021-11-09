# install.packages("readr")
# install.packages("ggplot2")

library("readr")
library("ggplot2")

Data<-read.csv("cars.csv")

## Basic information

attributes(Data)#List your attributes within your data set.

summary(Data) #Prints the min, max, mean, median, and quartiles of each attribute.

str(Data) #Displays the structure of your data set.

names(Data) #Names your attributes within your data set.

# Data$ColumnName Will print out the instances within that particular column in your data set.
Data$speed.of.car

## Plots

# Histogram Plot

hist(Data$speed.of.car)

# Scatter (Box) Plot

plot(Data$distance.of.car,Data$speed.of.car)

# Normal Quantile Plot- is a way to see if your data is normally distributed.

qqnorm(Data$speed.of.car)


## Manage Data

# Do you see any data types that need changing within your data set? If so, how do 
# you convert data types? Converting data types is a helpful skill to learn for 
# this tutorial and future analyses. Here is an example 
# of how one would change a column’s data type within a data set:

#Data$ColumnName<-as.typeofdata(Data$ColumnName)


# Do the columns/attributes within your dataset need renaming?
# Pick short names, so you’ll not have typing/spelling errors. You’ll also want
# to name your columns in order as they appear in your dataset.
# To rename the attributes/columns in your dataset, you'll want to use the
# c() function, specifying a name for each column. 

names(Data)<-c("name","speed","dist") 


# Do any of your variables have missing values? How do you know if your dataset 
# has any missing values? If you do not address missing values certain functions 
# will not work properly, so it’s smart to start the practice checking for
# missing values.  R labels missing as NA (Not Available). 
# Here are two ways to know if you have any missing values:

summary(Data) #Will count how many NA’s you have.

is.na(Data) #Will show your NA’s through logical data. (TRUE if it’s missing, 
# FALSE if it’s not.)

# How to address missing values? There are multiple ways to confront missing 
# values in your dataset – all depend on how much they will affect your dataset.
# Here are a few options:

# Remove any observations containing missing data. (If the missing data is less
# than 10% of the total data and only after comparing the min/max of all 
# the features both with and without the missing data.)

# na.omit(Data$ColumnName) #Drops any rows with missing values and omits them forever.
        
# na.exclude(Data$ColumnName) #Drops any rows with missing values, but keeps 
# track of where they were.
        
# Replace the missing values with the mean, which is common technique, but 
# something to use with care with as it can skew the data.
        
# Data$ColumnName[is.na(Data$ColumnName)]<-mean(Data$ColumnName,na.rm = TRUE)


## Creating Testing and Training sets.


set.seed(123)


# How do you split the data into training and test sets? You’ll now want to split
# your data into two sets for modeling. One is the training set and the other
# one being the test set. A common split is 70/30, which means that 70% of the 
# data will be the training set’s size and 30% of the data will be the test 
# set’s size. You will be using the 70/30 split, but another common split is 80/20.

# Setting the training set’s size and the testing set’s size can be done by 
# performing these two lines of code. These two lines calculate the sizes 
# of each set but do not create the sets:
  
trainSize<-round(nrow(Data)*0.7) 

testSize<-nrow(Data)-trainSize

trainSize

testSize


# How do you create the training and test sets? It’s now time for you to create 
# the training and test sets. We also want these sets to be in a randomized order,
# which will create the most optimal model.

# To perform this, you need to run these three lines of code. Type in this code
# into R Script or Console:
  
training_indices<-sample(seq_len(nrow(Data)),size =trainSize)

trainSet<-Data[training_indices,]

testSet<-Data[-training_indices,] 


# You’re now ready to run your data through your modeling algorithm. The model 
# that we will be using is the Linear Regression Model, which is helpful when 
# trying to discover the relationship between two variables. These two variables 
# represent the X and Y within the linear equation. The X variable is the predictor 
# variable, also known as the independent variable because it doesn’t depend on 
# other attributes while making predictions. Y is the response variable, also 
# known as the dependent variable because its value depends on the other variables. 
# (We will be keeping this at a high level. If you’d like to discover more about 
# this equation, please feel free to do your own research.) In our case, these 
# two variables will be Speed and Distance. We are trying to predict Distance, 
# so it is our dependent/response/Y variable. Speed is our independent/predictor/X 
# variable. 

# To create this model, we will be using the linear model function – lm(). Here
# is the basic line of code for the 
# linear model function. 

LRmodel<-lm(dist~ speed, trainSet)

# The code above is without any parameter’s or adjustments. If you’d like to 
# experiment with options, this is a perfect time to do so. Did you create an
# optimal model? To see key metrics of your model, type in this code into R Script
# or Console:
  
summary(LRmodel)


## Predictions
# The next step is to predict the cars distances through the speed of the cars.
# To do this, we’ll be using the
# prediction function – predict() 

PredictLR <- predict(LRmodel,testSet)

# To view your predictions, type in this code into R Script or Console:
  
PredictLR

## We represent the model vs the scatter plot of the data.
ggplot(data = Data, aes(x = speed, y = dist)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
