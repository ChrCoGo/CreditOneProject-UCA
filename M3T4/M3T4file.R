## Module 3 Task 4

#install.packages("arules")
#install.packages("arulesViz")

library(corrplot)
library(caret)
library(gbm)

library(reshape2) # Heatmap with melt()
library(ggplot2)

library(DataExplorer) # quick EDA package

library(arules)
library(arulesViz)

set.seed(123)



## Load, prepare and visualize the data:

# Electronidex's data contains rows that represent single transactions with the
# purchased item(s) being separated by commas, which is also called a 'basket" 
# format. RStudio does not inherently understand transactional data. If the 
# dataset is uploaded using the read.csv () function, RStudio will try to create 
# variables (remember your dataset doesn't have variables) and you will 
# encounter problems with your analysis. Therefore, we need to upload the 
# ElectronidexTransactions.csv through the read.transactions() function.

Data<-read.transactions("ElectronidexTransactions2017.csv", format = "basket",
                        sep=",", rm.duplicates=TRUE)

# The read.transactions() function changes the dataset into a sparse matrix.
# It makes each row represent a transaction and creates columns for each item 
# that a customer might purchase. Electronidex sells 125 items, so the sparse
# 3 matrix creates 125 columns. It also changes the data to binary.
# (1=item purchased in that transaction OR 0=no purchase.)


## We put comments to not show all the long data.
## we could algo inspect a sample.

# inspect(Data) # We can view the transactions.
length (Data) # Number of transactions.
# size (Data) # Number of items per transaction
# LIST(Data) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Data)# To see the item labels. in this case, all the different products


## Here we show in order the top 10 most sold products. 
itemFrequencyPlot(Data, topN=10) 

## In the plot we can see the iMac as the top 1 most sold, followed by HP Laptop,
## etc. Those are important products, as they will be the targets or Right Hand 
## Side (rhs) of our rules. 


## Here we represent a sample of the sparse matrix.
image(sample(Data,200))

## We can see that we have some accumulation of points forming some columns
## for example, near the 70th item. Precisely, we can see that the 70th item label
## is for the iMac, the most sold product. Analogously, other Apple products or
## the Acer Desktop produce those columns in the first 20 spots. Thus, those 
## columns represent the items sold most frequently. 


## Creating the Rules

## Here we have made different experiments, and we will pu a 4% of support. 
## The reason is that our bigger supports are near 5%. We also put a 
## 30% confidence, because our best confidence hardly surpass 
## 40%, which is not so high, but may be enough to discover some trends. 


RulesName<-apriori(Data, parameter = list(supp = 0.04, conf = 0.30, maxtime=10))
RulesName

inspect(RulesName)

summary(RulesName)

## We order the Rules by confidence and support. The top one has near 45% of 
## confidence. Also, observe that the higher support is a 7.5%.

inspect(sort(RulesName,by="confidence"))
inspect(sort(RulesName,by="support"))

## We check that there are not redundant rules.
is.redundant(RulesName)


## We represent visually with a plot and a graph. 
plot(RulesName)

plot(RulesName, method="graph", control=list(type="items")) 


## Conclusions and Insights:

# iMacs has the greater frequency, greater than 25%. As we can see from the 
# A Priori Rules, it is usually bough with other products, like other computers
# or monitors. We may deduce that the customers are looking to complement
# its ecosystem by buying both Windows and MacOS computers.

# The trend described about make sense when we think on a company as a customer.
# For example, other  products (for example, from Apple) appear in the top 
# frequencies but do not appear in the rules. Probably, because those products
# (macbooks or earpods) are more oriented to a final user, instead of a company 
# buying hardware.

# Greater confidence values came with the ViewSonic Monitor (into iMac or HP
# Laptop) This makes a lot of sense. You cannot use a monitor without a computer.

# Lift is a metric that quantifies the relation between lhs and rhs.  All are
# greater than 1, which implies positive correlation. The greater values
# of the lift appear with the ViewSonic Monitor as lhs, which make sense according
# to the logic described above with the monitors.

# Best products to Blackwell can be precisely the ones in the top 10 solds, but
# specifically those that appear repeatedly in the rules, as you can play with 
# those cross-selling patterns (iMac, HP Laptop, CYBERPOWER Gamer Desktop, 
# Dell Desktop, ViewSonic Monitor,...).

# Notice that those rules, in general, are not very strong. As we said before,
# supports are above 4-8% and confidence is not greater than 45%, but in this 
# context those numbers may be enough to reveal some trends. 
















