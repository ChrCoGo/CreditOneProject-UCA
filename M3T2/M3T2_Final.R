## Module 3 Task 2. We load Complete responses, explore a bit the data,
# then optimize 2 models:
# 1. GBM, Automatic Tuning.
# 2. Random Forest, Manual mtry values.
# We analyze both models and use it to predict the brands in the incomplete 
# survey.

#load library and set seed
library(caret)
library(gbm)
set.seed(998)

CompleteResponses <- read.csv("CompleteResponses.csv")

CompleteResponses$brand=as.factor(CompleteResponses$brand)

Incomplete <- read.csv("SurveyIncomplete.csv")

summary(CompleteResponses) #Prints the min, max, mean, median, and quartiles of each attribute.

str(CompleteResponses) #Displays the structure of your data set.

names(CompleteResponses) #Names your attributes within your data set.


#create a 20% sample of the data
#data <- data[sample(1:nrow(data), 7000,replace=FALSE),]


# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


## Here we include the First Algorithm: Stochastic Gradient Boosting (10-fold
# cross-validation and Automatic Tuning Grid)

#train model with a tuneLenght = 5 (trains with 5 mtry value for Stochastic
#Gradient Boosting)
gmbFitm0 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 5);

gmbFitm0


summary(gmbFitm0) # Shows the relative influence of each variable in the model.

VarImpGBM<-varImp(gmbFitm0,numTrees = NULL) # variable importance

VarImpGBM

# First, we will evaulate on the testing set to evaluate

gmbPredict1 <- predict(gmbFitm0,testing)# The values are discrete because
# the imputs were introduced as factors. In other case, the model returns
# continuous values, close to 0 and 1.

gmbPredict1 

summary(gmbPredict1)


Results1gmb<-postResample(gmbPredict1,testing$brand)# Show accuracy and kappa.

Results1gmb  # over 92% of accuracy, 0.83 kappa.


# Prediction of the Incomplete Surveys.

gmbPredictBrand <- predict(gmbFitm0,Incomplete)

gmbPredictBrand 

summary(gmbPredictBrand) # 1940 Acer, 3060 Sony.



#Random Forest model. With 5 manual values for mtry.


#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid)

#training results
rfFitm1

summary(rfFitm1)

VarImprf <- varImp(rfFitm1)
VarImprf



rfPredict1 <- predict(rfFitm1,testing)# The values are discrete because
# the imputs were introduced as factors. In other case, the model returns
# continuous values, close to 0 and 1.

rfPredict1 

summary(rfPredict1)

Results1rf<-postResample(rfPredict1,testing$brand)# Show accuracy and kappa.

Results1rf # almost 92% of accuracy, 0.82 kappa.


# Prediction of the Incomplete Surveys.

rfPredictBrand <- predict(rfFitm1 ,Incomplete)

rfPredictBrand

summary(rfPredictBrand) # 1905 Acer, 3095 Sony.


