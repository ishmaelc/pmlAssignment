#### Prediction work ####

### import CSV data into R
preData <- read.csv('http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv')

#### caret package required
library(caret)

#### Seed set for reporducible results
set.seed(3433)

#### strings identified through exploratory data analysis
#### as having incomplete data
strings <- c('kurtosis','skewness','max','min','amplitude','avg','var','stddev')

#### run a loop to search and identify all the field names which match the 
#### items identified in the strings vector. Remove these fields from the dataset
toss <- NULL
for (i in strings) 
  toss <- c(toss, grep(pattern = i, names(preData), fixed = T))
preData <- preData[,-toss]

#### remove subject data fields
preData <- preData[,-(1:7)]

#### split data into training and testing, used 25% of data for training
inTrain <- createDataPartition(preData$classe, p = .25)[[1]]
training <- preData[ inTrain,]
testing <- preData[-inTrain,]

# train data to a random forest method
modelFit <- train(classe ~ .,method = 'rf', data= training)

#run training through model fit and display confusion matrix
trainCM <- confusionMatrix(training$classe, predict(modelFit,training))

#run training through model fit and display confusion matrix
testCM <- confusionMatrix(testing$classe, predict(modelFit,testing))

trainCM
testCM
