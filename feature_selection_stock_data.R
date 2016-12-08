getwd()
setwd('C:/Users/ch186802/R')

#packages required
#install.packages('Metrics')
library('Metrics')

#install.packages('randomForest')
library('randomForest')

#install.packages('ggplot2')
library('ggplot2')

#install.packages('ggthemes')
library('ggthemes')

#install.packages('dplyr')
library('dplyr')

#install.packages('Hmisc')
library(Hmisc)

#set random seed
set.seed(101)

#loading stock dataset
data<-read.csv("stock_data.csv",stringsAsFactors= T)

#exploring the data
dim(data)
summary(data)
colnames(data)
describe(data)

#appears that data has abt 3000 rows with about 101 attributes
data$Y <- as.factor(data$Y)

#creating a new attribute
data$Time <- NULL

#subsetting the data into a train and test data sets
train <- data[1:2000,]
test <- data[2001:3000,]

# applying Random forest on the training dataset 
# and getting a prediction
rf_model <- randomForest(Y ~ ., data = train)
preds <- predict(rf_model, test[,-101])
table(preds)

#now checking the accuracy
auc(preds, test$Y)

#0.452 is not a great accuracy so trying to improve
#it by implementing it only on the top 20 features

#checking the feature importance
imp <- importance(rf_model)
colnames(imp)

#picking features with a importance score >= 10.5
imp[imp>10.6]
imp>10.6
summarise(imp>10.56)

imp_subset <- subset(imp, imp > 10.56)
nrow(imp_subset)
colnames(imp_subset)
table(imp_subset)
imp_subset

rf_model <- randomForest(Y ~ X2+ X6+ X7+ X11+ X12+ 
                           X15+ X24+ X29+ X30+ X31+ 
                           X37+ X40+ X55+ X58+ X64+
                           X66+ X75+ X89+ X90+ X98,
                         data = train)

preds <- predict(rf_model, test[,-101])
table(preds)

#now checking the accuracy
auc(preds, test$Y)

"
by selectign the top 20 features from the data set
we have:
1. increased the accuracy of the predictive model from
    0.452 to 0.475
2. reduced the comlpexity of the model
3. reduced the training time of the model
"

