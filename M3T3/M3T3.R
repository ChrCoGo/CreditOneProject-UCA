# Module 3 Task 3: Predicting Customers Preference
library(corrplot)
library(caret)
library(gbm)

library(reshape2) # Heatmap with melt()
library(ggplot2)

library(DataExplorer) # quick EDA package


set.seed(998)

existing_products<- read.csv("existingproductattributes2017.csv")

summary(existing_products) # Shows that in BestSellersRank we have NA's.

# Delete the attribute with missing data:
existing_products$BestSellersRank<-NULL 

# dummify the data

EPdata <- dummyVars(" ~ .", data = existing_products)

readyEPdata <- data.frame(predict(EPdata, newdata = existing_products))

# quick EDA:

plot_str(readyEPdata)

plot_missing(readyEPdata)

plot_histogram(readyEPdata)

plot_density(readyEPdata)

#plot correlation matrix in two different ways:

plot_correlation(readyEPdata, type = 'continuous')


# We compute correlation 
corrData <- cor(readyEPdata)


melted_cormat <- melt(corrData)
head(melted_cormat)

#ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
 # geom_tile()

corrplot(corrData) # heat map correlation matrix.

# From the correlation Matrix we can observe that one of the variables 
# that has a higher value from the positive correlation is the x5StarReviews.
# We may want to build the linear model using this as a feature:




#############################
# Linear Model 1: Predicting Volume using 5 Star Reviews

Data<-readyEPdata
# We select the features according to variable importance and the correlation matrix.
Data<-Data[c("x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews","x1StarReviews","PositiveServiceReview","NegativeServiceReview","Volume")]


trainSize<-round(nrow(Data)*0.7) 

testSize<-nrow(Data)-trainSize

trainSize

testSize



training_indices<-sample(seq_len(nrow(Data)),size =trainSize)

trainSet<-Data[training_indices,]

testSet<-Data[-training_indices,]

LRmodel<-lm(Volume~ x5StarReviews, trainSet)

summary(LRmodel)


PredictLR <- predict(LRmodel,testSet)


PredictLR

## We represent the model vs the scatter plot of the data.
ggplot(data = Data, aes(x = x5StarReviews, y = Volume)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


# Apparently there is nothing wrong, but it is very suspicious having practically perfect
# scores even using only one variable (even though, the correlation matrix
# show a very high value of positive correlation). It may be a case of overfitting.







# Linear Model 2: Predicting Volume using 4 Star Reviews


LRmodel<-lm(Volume~ x4StarReviews, trainSet)

summary(LRmodel)


PredictLR <- predict(LRmodel,testSet)


PredictLR

## We represent the model vs the scatter plot of the data.
ggplot(data = Data, aes(x = x4StarReviews, y = Volume)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


# $ Star also has a high correlation value and the results are not even close 
# to the 5StarReviews case, but this may be a more realistic result.


## Non-parametric Machine Learning Models


# define an 70%/30% train/test split of the dataset
inTraining <- createDataPartition(Data$Volume, p = .70, list = FALSE)
training <- Data[inTraining,]
testing <- Data[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

############################################################################
## Support Vector Machines (SVM)


#train model with a tuneLenght = 5 (trains with 5 mtry value for SVM)
SVMfit1 <- train(Volume~., data = training, method = "svmLinear", trControl=fitControl, tuneLength = 5);

SVMfit1


summary(SVMfit1) # Shows the relative influence of each variable in the model.

VarImpSVM<-varImp(SVMfit1,numTrees = NULL) # variable importance

VarImpSVM

# First, we will evaulate on the testing set to evaluate

SVMpred1 <- predict(SVMfit1,testing)
SVMpred1 # It predicts negative values of Volumes, which is impossible. We should
# discard this model

summary(SVMpred1)


ResultsSVM1<-postResample(SVMpred1,testing$Volume)#

ResultsSVM1 # #Rsquared is very high, but RMSE and MAE may be suspicious.

###########################################################################

## Random Forest

#train model with a tuneLenght = 5 (trains with 5 mtry value for Random Forest)
RFfit1 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 5);

RFfit1


summary(RFfit1) # Shows the relative influence of each variable in the model.

VarImpRF<-varImp(RFfit1,numTrees = NULL) # variable importance

VarImpRF

# First, we will evaulate on the testing set to evaluate

RFpred1 <- predict(RFfit1,testing)

RFpred1 # Every volume predicted is a positive number, as it should be.

summary(RFpred1)


ResultsRF1<-postResample(RFpred1,testing$Volume)#

ResultsRF1 #

#################################################
## Gradient Boosting


#train model with a tuneLenght = 5 (trains with 5 mtry value for Random Forest)
GBfit1<-train(Volume~., data = training, method = "gbm", trControl=fitControl, tuneLength = 5,verbose=FALSE)

GBfit1


summary(GBfit1) # Shows the relative influence of each variable in the model.

VarImpGB<-varImp(GBfit1,numTrees = NULL) # variable importance

VarImpGB

# First, we will evaulate on the testing set to evaluate

GBpred1 <- predict(GBfit1,testing)# The values are discrete because
# the imputs were introduced as factors. In other case, the model returns
# continuous values, close to 0 and 1.

GBpred1 #Negative Volumes. We don't keep this model.

summary(GBpred1)


ResultsGB1<-postResample(GBpred1,testing$Volume)#

ResultsGB1 #


# Random Forest is the only one that does not predict any negative Volumes,
# and has one of the better performance. Thus, we will use Random Forest to 
# do the final prediction.


new_products<- read.csv("newproductattributes2017.csv")


summary(new_products) # Shows that in BestSellersRank we have NA's.

# Delete the attribute with missing data:
new_products$BestSellersRank<-NULL 

new_products$Volume<-NULL #Delete the empty Volume Variable

# dummify the data

NPdata <- dummyVars(" ~ .", data = new_products)

readyNPdata <- data.frame(predict(NPdata, newdata = new_products))

Ndata<-readyNPdata[c("x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews","x1StarReviews","PositiveServiceReview","NegativeServiceReview")]
  

FinalPred <- predict(RFfit1,Ndata) # We predict with RF model.

FinalPred<-round(FinalPred)

FinalPred

## Prepare the output. We will leave the original file intact, adding the predictions.

output<-read.csv("newproductattributes2017.csv")

output$predictions<-FinalPred

write.csv(output, file="C2.T3output.csv", row.names = TRUE)


##############################

## Conclusions:

# In the correlation matrix we could appreciate that the reviews and opinions of
# the customers have a great impact on the Volume of sales. Specifically,
# the 5 4 and 3 Stars reviews and the Positive Service Reviews. Accordingly, those
# are the variables with high relative importance when building the models. 

# We can get nice results by reducing the number of features. As it is shown by
# the correlation matrix, the majority of the variables are just very minorly
# related with sales Volumes. 

# A sample of 80 to test is not so big. We may want a bigger dataframe.
# Linear Regression, GBM and SVM were used and even provided very nice Rsquare values.
# but were rejected because overfitting or predicting negative volumes.
# RF provided 0.92 Rsquared and apparently good results. Also, every prediction
# was a non-negative value. 

# We focus on PC, laptops, Netbooks and SmartPhones. In the final .csv  output
# with Excel we can seethe predictions on the volums of this and other products:

# Total Volumes Smartphones: 1304. The Product Number 194 has 504 of volume sales.

# Total Volumes Netbooks: 1483. ProdNum 180 has a predicted volume of 1241, so apparently
# it will be nice product to sell. Probably because it is the cheapest Netbook. 
# 9% Margin of profit. 

# Total Volumes Laptop: 344.The product number 173 has 309 predicted sells. 10%,
# the smalles profit margin on laptops.

# Total Volumes PC: 610. The product number 171 has 420 sales by its own, and 
# also has a 25% of profit margin. Very nice product from the economic perspective. 






