library(caret)
library(randomForest)
library(rattle)
library(rpart)

set.seed(1)

if (!file.exists("./data/pml-training.csv")) {
    trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(trainingUrl, destfile="./data/pml-training.csv", method="curl")
}

if (!file.exists("./data/pml-test.csv")) {
    testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(testingUrl, destfile="./data/pml-testing.csv", method="curl")
}

training <- read.csv("./data/pml-training.csv", header=TRUE, na.strings=c("NA","#DIV/0!",""))
test <- read.csv("./data/pml-testing.csv", header=TRUE, na.strings=c("NA","#DIV/0!",""))

# remove first and fifth columsn from training
# these are ids and a factor timestamp
training <- training[,c(-1, -5)]

# remove any columns that contain entirely NAs
training <- training[, colSums(is.na(training)) == 0]

# check for columns with near zero variance
trainingNZV <- nearZeroVar(training, saveMetrics=TRUE)
colnames(training[trainingNZV$nzv == TRUE])

training <- training[trainingNZV$nzv == FALSE]
dim(training)

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining)
dim(myTesting)

rpartModFit <- rpart(classe ~ ., data=myTraining, method="class")
fancyRpartPlot(rpartModFit)

rpartPredictions <- predict(rpartModFit, myTesting, type="class")
confusionMatrix(rpartPredictions, myTesting$classe)

rfModFit <- randomForest(classe ~ ., data=myTraining)
rfPredictions <- predict(rfModFit, myTesting, type="class")
confusionMatrix(rfPredictions, myTesting$classe)

test <- test[, colnames(test) %in% colnames(training)]
colnames(training)
colnames(test)
str(training)
str(test)

testPredictions <- predict(rfModFit, test, type="class")
# testPredictions