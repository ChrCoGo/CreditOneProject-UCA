# install.packages("readr")
# install.packages("ggplot2")

library("readr")
library("ggplot2")

IrisDataset <- read.csv("iris.csv")

attributes(IrisDataset)

summary(IrisDataset) 

str(IrisDataset)

names(IrisDataset)

barplot(table(IrisDataset$Species)) # We cannot plot or make a histogram about 
# the Species column. I find the the natural representation should be a count
# on the number of each species.

plot(IrisDataset$Sepal.Length)

# there is no special need to change the species into numeric values for this task.
# But we can do it with:

IrisDataset$Species<- as.numeric(as.factor(IrisDataset$Species)) 

# Represent the normality of the feature
qqnorm(IrisDataset$Petal.Width)


# Represent the normality of the target
qqnorm(IrisDataset$Petal.Length)

     

trainSize <- round(nrow(IrisDataset) * 0.2)
     
testSize <- nrow(IrisDataset) - trainSize
     
trainSize
     
testSize


set.seed(123)
     
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)
     
trainSet <- IrisDataset[training_indices,]
     
testSet <- IrisDataset[-training_indices,]
     
LinearModel<- lm(Petal.Length~ Petal.Width, trainSet)
     
summary(LinearModel) # R2 score = 0.95, p-value < 2.2e-16. Good enough prediction. 
     
prediction<-predict(LinearModel,testSet)
     
prediction

## We represent the model vs the scatter plot of the data.
ggplot(data = IrisDataset, aes(x = Petal.Width, y = Petal.Length)) +
geom_point() +
stat_smooth(method = "lm", col = "dodgerblue3") +
theme(panel.background = element_rect(fill = "white"),
axis.line.x=element_line(),
axis.line.y=element_line()) +
ggtitle("Linear Model Fitted to Data")

