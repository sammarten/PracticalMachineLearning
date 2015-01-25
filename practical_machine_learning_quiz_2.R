# Practical Machine Learning - Quiz 2

# Question 2
library(AppliedPredictiveModeling)
data(concrete) 
library(caret) 
set.seed(975) 
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]] 
training = mixtures[ inTrain,] 
testing = mixtures[-inTrain,]

q <- 3

# 1
training$Cement.level <- cut2(training$Cement, g=q)
plot(training$CompressiveStrength, col=training$Cement.level, main="Cement")
plot(training$Cement, col=training$Cement.level, main="Cement")
# 2
training$BlastFurnaceSlag.level <- cut2(training$BlastFurnaceSlag, g=q)
plot(training$CompressiveStrength, col=training$BlastFurnaceSlag.level, main="Blast Furnace Slage")
plot(training$BlasFurnaceSlag, col=training$BlastFurnaceSlag.level, main="Blast Furnace Slage")

# 3
training$FlyAsh.level <- cut2(training$FlyAsh, g=q)
plot(training$CompressiveStrength, col=training$FlyAsh.level, main="Fly Ash")
plot(training$FlyAsh, col=training$FlyAsh.level, main="Fly Ash")

# 4
training$Water.level <- cut2(training$Water, g=q)
plot(training$CompressiveStrength, col=training$Water.level, main="Water")

#5
training$Superplasticizer.level <- cut2(training$Superplasticizer, g=q)
plot(training$CompressiveStrength, col=training$Superplasticizer.level, main="Superplasticizer")

#6
training$CoarseAggregate.level <- cut2(training$CoarseAggregate, g=q)
plot(training$CompressiveStrength, col=training$CoarseAggregate.level, main="Course Aggregate")

#7
training$FineAggregate.level <- cut2(training$FineAggregate, g=q)
plot(training$CompressiveStrength, col=training$FineAggregate.level, main="Fine Aggregate")

#8
training$Age.level <- cut2(training$Age, g=q)
plot(training$CompressiveStrength, col=training$Age.level, main="Age")
plot(training$Age, col=training$Age.level, main="Age")

# Alternative
plot(training$CompressiveStrength, col=cut2(training$Age, g=4), main="Age")

# Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
plot(training)
plot(training$Superplasticizer)
log(0)

# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
col <- grep("^IL", names(training), value = TRUE)
t <- subset(training, select=col)

# Alternative
# t <- training[,col]
# t <- training[inTrain, grep("^IL", names(training), value = TRUE)]

preProcess(t, method="pca", thresh=0.8)

# Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

training <- training[,grep("^IL", names(training), value = TRUE)]
testing <- testing[,grep("^IL", names(testing), value = TRUE)]

training <- cbind(diagnosis[inTrain], training)
colnames(training)[1] <- c("Diagnosis")

testing <- cbind(diagnosis[-inTrain], testing)
colnames(testing)[1] <- c("Diagnosis")

# PCA
preProc <- preProcess(training[,-1], method="pca", thresh=0.8) # -1 removes diagnosis column
trainPC <- predict(preProc, training[,-1])
modelFit <- train(training$Diagnosis ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, testing[,-1])
confusionMatrix(testing$Diagnosis, predict(modelFit, testPC))

# No Preprocessing
modelFit <- train(Diagnosis ~ ., data=training, method="glm")
predictions <- predict(modelFit, newdata=testing)
confusionMatrix(predictions, testing$Diagnosis)



