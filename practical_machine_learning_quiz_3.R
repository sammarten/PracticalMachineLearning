# Question 1

library(AppliedPredictiveModeling) 
data(segmentationOriginal) 
library(caret)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.
training <- subset(segmentationOriginal, Case == "Train")
testing <- subset(segmentationOriginal, Case == "Test")

# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
set.seed(125)
modFit <- train(Class ~ ., method="rpart", data=training)
modFit$finalModel
library(rattle)
fancyRpartPlot(modFit$finalModel)

# 3. In the final model what would be the final model prediction for cases with the following variable values:
#   a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
#       23000 less than 45000
#       PS
#   b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#       50000 > 45000
#       10 > 9.7
#       WS
#   c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
#       57000 > 45000
#       8 < 9.7
#       PS
#   d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
#       Don't know since TotlaIntentCh2 not provided

# Question 3

library(pgmm)
data(olive)
olive = olive[,-1]

library(tree)
modFit <- tree(Area ~ ., data=olive)

# t() gives the transpose
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata) # 2.875


# Question 4

# Load the South Africa Heart Disease Data and create training and test sets 
# with the following code:

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Set the seed to 13234 and fit a logistic regression model (method="glm", 
# be sure to specify family="binomial") with Coronary Heart Disease (chd) 
# as the outcome and age at onset, current alcohol consumption, obesity 
# levels, cumulative tabacco, type-A behavior, and low density lipoprotein 
# cholesterol as predictors. Calculate the misclassification rate for your 
# model using this function and a prediction on the "response" scale.

set.seed(13234)
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                method="glm", 
                family="binomial", 
                data=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(modFit, trainSA)) # [1] 0.2727273
missClass(testSA$chd, predict(modFit, testSA)) # [1] 0.3116883

# Question 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
modFit <- randomForest(y ~ ., data=vowel.train)
varImp(modFit)


