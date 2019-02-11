

install.packages("caret")
install.packages("corrplot")
install.packages("e1071")
install.packages("lattice")

library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(lattice)
sessionInfo() # проверили наличие пакетов

data("segmentationOriginal")
summary(segmentationOriginal)

segData <- subset(segmentationOriginal, Case == "Train")

Cell <- segData$Cell
Class <- segData$Class
Case <- segData$Case

segData <- segData[, -(1:3)]

statusColNum <- grep("Status", names(segData)) #берет номера столбиков, в названии которых есть Status
segData <-segData[, -statusColNum]

skewness(segData$AreaCh1) # один из хвостов длиннее другого
skewness(segData$AngleCh1)

skewValues <- apply(segData, 2, skewness)
skewValues
head(skewValues)
mean(skewValues)

hist(segData$AngleCh1, breaks=50)
hist(segData$KurtIntenCh1, breaks=50)
mean(skewValues)

hist(segData$AreaCh1, breaks=50)

Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

#predict(Ch1AreaTrans, head(segData$AreaCh1))
a <- predict(Ch1AreaTrans, segData$AreaCh1)
hist(a, breaks=30)
skewness(a)
shapiro.test(a)$p #ненормальное распределение получилось

pcaObject <- prcomp(segData, center= TRUE, scale. = TRUE)
pcaObject

# WORKSHOP 2


# CARET: preProcess function and predict function:
# predict(Model, Data)
# Model = SVM, Neural ... (on previous step)

trans <- preProcess(segData, method =c("BoxCox", "center", "scale", "pca"))
trans
transformed <- predict(trans, segData)
head(transformed[, 1:5])

segTrain <- subset(segmentationOriginal, Case == "Train")
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class
segPP <- preProcess(segTrainX, method = "BoxCox")
segTrainTrans <- predict(segPP, segTrainX)

xyplot(AvgIntenCh1 ~ EntropyIntenCh1,
       data = segTrainTrans,
       groups = segTrainClass,
       xlab = "Channel 1 Fiber Width",
       ylab = "Intensity Entropy Channel 1",
       auto.key = list(columns = 2),
       type = c("p", "g"),
       #fill = c("p", "g"),
       main = "Original Data",
       aspect = 1)

pr <- prcomp(~ AvgIntenCh1 + EntropyIntenCh1, 
             data = segTrainTrans, 
             scale. = TRUE)

xyplot(PC2 ~ PC1,
       data = as.data.frame(pr$x),
       groups = segTrain$Class,
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Transformed",
       xlim = extendrange(pr$x),
       ylim = extendrange(pr$x),
       type = c("p", "g"),
       aspect = 1)

# это просто крутяшка для красивости
transparentTheme(pchSize = .8, trans = .3)

isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
sum(isZV)
segTrainX <- segTrainX[, !isZV]
segPP <- preProcess(segTrainX, c("BoxCox", "center", "scale", "pca"))
segTrainTrans <- predict(segPP, segTrainX)
segPCA <- prcomp(segTrainTrans, center = TRUE, scale. = TRUE)
panelRange <- extendrange(segPCA$x[, 1:3])
splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)

# Scree plot w/o PreProcess (works!)
segPP2 <- preProcess(segData, c("BoxCox", "center", "scale", "pca"))
segPCA <- prcomp(segData, center = TRUE, scale. = TRUE)

segPCA_vs_Prcomp <- predict(segPP2, segData)
#screeplot(segPCA_vs_Prcomp, type="lines", npcs=58)
screeplot(segPCA, type="lines", npcs=58)

segPP2
# PCA needed 19 components to capture 95 percent of the variance

segPCA

#Correlations

nearZeroVar(segData)
correlations <- cor(segData)
dim(correlations)
correlations[1:5, 1:5]

corrplot(correlations, order="hclust")

highCorr <- findCorrelation(correlations, cutoff=.75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

#install.packages("car")
library(car)
#VIF is in the car package

data(cars)
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$Type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))

carSubset <- cars[sample(1:nrow(cars), 20), c(1, 2, 19)]

head(carSubset)
levels(carSubset$Type)

simpleMod <- dummyVars(~Mileage + Type,
                       data = carSubset,
                       levelsOnly = TRUE)
simpleMod
MY_DUMMY1 <-predict(simpleMod, carSubset)

withInteraction <- dummyVars(~Mileage + Type + Mileage:Type,
                             data = carSubset,
                             levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(carSubset))


#Workshop 3

x=1:12
sample(x)
sample(x,replace=TRUE)
sample(x,replace=T,size=10)

library(AppliedPredictiveModeling)
data(twoClassData)

str(predictors)
str(classes)

set.seed(1)
trainingRows = createDataPartition (classes, p=.80, list=FALSE)
head(trainingRows)

trainingPredictors <- predictors[trainingRows,]
trainClasses <- classes[trainingRows]

testPredictors<-predictors[-trainingRows,]
testClasses<-classes[-trainingRows]

#Repeated splits

set.seed(1)
repeatedSplits<-createDataPartition(trainClasses,p=.80,times=3)
str(repeatedSplits)


#Bootstrap(does exist)

#k-fold cross-validation
set.seed(1)
cvSplits=createFolds(trainClasses,k=10,returnTrain = TRUE)
str(cvSplits)

fold1=cvSplits[[1]]
cvPredictors1=trainingPredictors[fold1,]
cvClasses1=trainClasses[fold1]

nrow(trainingPredictors)
nrow(cvPredictors1)

#modelFunction(price~numBedrooms+numBaths+acres,data=housingData)
#or modelFunction(price~.,data=housingData) -for all fiels


#knn3 function

trainingPredictors=as.matrix(trainingPredictors)
knnFit=knn3(x=trainingPredictors,y=trainClasses, k=5)

knn3(x=trainingPredictors,y=trainClasses, k=5)


testPredictions=predict(knnFit,newdata = testPredictors,type="class")
head(testPredictions)
str(testPredictions)

xkcd=cbind(testPredictors,testPredictions)

#Tuning parameters

library(caret)
data(GermanCredit)

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

set.seed(100)
inTrain=createDataPartition(GermanCredit$Class,p=.8)[[1]]
GermanCreditTrain=GermanCredit[inTrain, ]
GermanCreditTest=GermanCredit[-inTrain, ]

set.seed(1056)
svmFit=train(Class~.,data=GermanCreditTrain,method="svmRadial")

#Classify
set.seed(1056)
svmFit=train(Class~.,data=GermanCreditTrain,method="svmRadial",
             preProc=c("center","scale"),
             tuneLength=10,
             trControl=trainControl(method="repeatedcv",
                                    repeats=5,
                                    classProbs = TRUE))
svmFit$results

#predict new samples

predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)

plot(svmFit)

#end of workshop3


#Workshop4

 
library(AppliedPredictiveModeling)
data(solubility)

trainingData<-solTrainXtrans
trainingData$Solubility<-solTrainY

library(caret)

lmFitAllPredictors<-lm(Solubility~.,data=trainingData)


lmPred1<-predict(lmFitAllPredictors,solTestXtrans)
head(lmPred1)

lmValues1<-data.frame(obs=solTestY,pred=lmPred1)
defaultSummary(lmValues1)


observed<-as.vector(solTestY)
predicted<-as.vector(lmPred1)

residualValues<-observed-predicted
summary(residualValues)

axisRange<-extendrange(c(observed,predicted))

plot(observed,predicted,ylim=axisRange,xlim = axisRange)
abline(0,1,col="darkgrey",lty=2)

plot(predicted,residualValues,ylab = "residual")
abline(h=0,col="darkgrey",lty=2)


RMSE(predicted,observed)
R2(predicted,observed)
cor(predicted,observed)

install.packages("MASS")
library(MASS)

rlmFitAllPredictors<-rlm(Solubility~.,data=trainingData)

lmPred2<-predict(rlmFitAllPredictors,solTestXtrans)

lmValues2<-data.frame(obs=solTestY,pred=lmPred2)

observed2<-as.vector(solTestY)
predicted2<-as.vector(lmPred2)

residualValues2<-observed2-predicted2

library(caret)
RMSE(predicted2,observed2)
R2(predicted2,observed2)
cor(predicted2,observed2)


ctrl<-trainControl(method = "cv",number = 10)

set.seed(100)
lmFt1<-train(x=solTrainXtrans,y=solTrainY,method = "lm",trControl = ctrl)
lmFt1

lmFt2<-train(x=solTrainX,y=solTrainY,method = "lm",trControl = ctrl)
lmFt2


lmPred33<-predict(lmFt2,solTestX)
summary(lmPred33)

lmValues3<-data.frame(obs=solTestY,pred=lmPred33)

observed3<-as.vector(solTestY)
predicted3<-as.vector(lmPred33)

residualValues3<-observed3-predicted3

library(caret)
RMSE(predicted3,observed3)
R2(predicted3,observed3)
cor(predicted3,observed3)


corThresh<-.9
tooHigh<-findCorrelation(cor(solTrainXtrans),corThresh)

corrPred<-names(solTrainXtrans)[tooHigh]
trainXfiltered<-solTrainXtrans[,-tooHigh]
testXfiltered<-solTestXtrans[,tooHigh]



lmPred44<-predict(lmFt2,testXfiltered)

lmValues4<-data.frame(obs=solTestY,pred=lmPred44)

observed4<-as.vector(solTestY)
predicted4<-as.vector(lmPred44)

residualValues4<-observed4-predicted4
library(caret)
RMSE(predicted4,observed4)
R2(predicted4,observed4)
cor(predicted4,observed4)

#Workshop5

install.packages('pls')
library('pls')
plsFit<-plsr(Solubility~.,data=trainingData)
plsFit$coefficients

predictedPLS=predict(plsFit,solTestXtrans,ncomp = 228)
typeof(predictedPLS)

##lmValues5<-data.frame(pred=as.vector(TestResultsPCA), obs= SolTestY)
##DefaultSummary(lmValues5)

set.seed(100)
library(caret)
plsTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:20),
                 trControl = ctrl)
plsTune

testResults=data.frame(ops=solTestY)
testResults$PLS <- predict(plsTune, solTestXtrans)


plsResamples <- plsTune$results
plsResamples$Model <- "PLS"

plsPlotData <- plsResamples
xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 1),
       groups = Model,
       type = c("o", "g"))

plot(plsTune$results[[3]])

observed5<-as.vector(solTestY)
predicted5<-as.vector(testResults$PLS)

residualValues5<-observed5-predicted5
library(caret)
RMSE(predicted5,observed5)
R2(predicted5,observed5)
cor(predicted5,observed5)

#PCR

set.seed(100)
pcrTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "pcr",
                 tuneGrid = expand.grid(ncomp = 1:35),
                 trControl = ctrl)
pcrTune                  

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))

testResultsPCR=data.frame(ops=solTestY)
testResultsPCR$PCR <- predict(pcrTune, solTestXtrans)

observed6<-as.vector(solTestY)
predicted6<-as.vector(testResultsPCR$PCR)
library(caret)
RMSE(predicted6,observed6)
R2(predicted6,observed6)
cor(predicted6,observed6)


xyplot(Rsquared ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "R^2 (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))
plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))


#ringe-regression models
install.packages('MASS')
install.packages('elasticnet')

library(MASS)
library(elasticnet)

ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))
set.seed(100)
library(caret)
ridgeTune <- train(x = solTrainXtrans, y = solTrainY,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"))
ridgeTune

print(update(plot(ridgeTune), xlab = "Penalty"))

enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
enetTune

plot(enetTune)

testResults$Enet <- predict(enetTune, solTestXtrans)

#workshop6

install.packages('doParallel')
library(AppliedPredictiveModeling)
library(doParallel)

#detectCores()
#cl <- makeCluster(detectCores())
cl <- makeCluster(3)
registerDoParallel(cl)

tooHigh<-findCorrelation(cor(solTrainXtrans),cutoff = .75)
trainXnnet=solTrainXtrans[,-tooHigh]
testXnnet=solTestXtrans[,-tooHigh]

library(caret)
set.seed(100)
indx <- createFolds(solTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

nnetGrid <- expand.grid(decay = c(0.1), 
                        size = c(11), 
                        bag = FALSE)
set.seed(100)
nnetTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = TRUE,
                  MaxNWts = 13 * (ncol(solTrainXtrans) + 1) + 13 + 1,
                  maxit = 50,
                  allowParallel = TRUE)

nnetTune

testResults <- data.frame(obs = solTestY,
                          pred = predict(nnetTune, solTestXtrans))
defaultSummary(testResults)


#WS7
library(AppliedPredictiveModeling)
data(solubility)
#install.packages("earth")
library(earth)
marsFit <- earth(solTrainXtrans, solTrainY)
marsFit
plotmo(marsFit)
library(caret)
marsGrid <-  expand.grid(.degree = 1:2, .nprune = 2:38)

set.seed(100)
marsTuned <- train(solTrainXtrans, solTrainY, method = "earth",
                   tuneGrid = marsGrid,
                   trControl = trainControl(method = "cv"))

testResultsMARS=data.frame(ops=solTestY)
testResultsMARS$MARS <- predict(marsTuned, solTestXtrans)

observedMA<-as.vector(solTestY)
predictedMA<-as.vector(testResultsMARS$MARS)
library(caret)
RMSE(predictedMA,observedMA)
R2(predictedMA,observedMA)
cor(predictedMA,observedMA)



# WS 9
library(AppliedPredictiveModeling)
set.seed(975)
simulatedTrain <-  quadBoundaryFunc(500)
simulatedTest <-  quadBoundaryFunc(1000)
head(simulatedTrain)
library(caret)
xyplot(X2~X1, data = simulatedTrain, groups = class)

#randomForest
#install.packages('randomForest')
library(randomForest)
randForModel <- randomForest(class~X1+X2, data = simulatedTrain, ntree = 2000)

library(MASS)
qdaModel <-  qda(class ~ X1+X2, data = simulatedTrain)

qdaTrainPred=predict(qdaModel, simulatedTrain)
names(qdaTrainPred)

head(qdaTrainPred$class)
head(qdaTrainPred$posterior)
qdaModel

qdaTestPred=predict(qdaModel, simulatedTest)
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]
simulatedTest$QDAclass <- qdaTestPred$class
#RandForest
rfTestPred <- predict(randForModel, simulatedTest, type = "prob") #вероятности, без type даст класс
simulatedTest$RFprob <- rfTestPred[,"Class1"]
simulatedTest$RFclass <- predict(randForModel, simulatedTest)

confusionMatrix(data = simulatedTest$RFclass, reference = simulatedTest$class, positive = "Class1")
confusionMatrix(data = simulatedTest$QDAclass, reference = simulatedTest$class, positive = "Class1")

#pROC
# rocCurve for RandomForest
install.packages("pROC")
library(pROC)
rocCurve <- roc(response = simulatedTest$class, predictor = simulatedTest$RFprob, 
                levels = rev(levels(simulatedTest$class)))
auc(rocCurve)
ci(rocCurve)
plot(rocCurve, legacy.axis = T)

#for QDA
rocCurve2 <- roc(response = simulatedTest$class, predictor = simulatedTest$QDAprob, 
                levels = rev(levels(simulatedTest$class)))
auc(rocCurve2)

#Calibrating Probabilities
calCurve=calibration(class~RFprob+QDAprob,data=simulatedTest)
calCurve

xyplot(calCurve, auto.key=list(columns=2))

#Grant success prediction
load("D:/testing.RData")
load("D:/training.RData")
str(training)
 

#################### WS 10

load('training.RData')
load('testing.RData')
load('pre2008Data.RData')
load('year2008Data.RData')
load('reducedSet.RData')
install.packages('pROC')
library(caret)
library(plyr)
library(reshape2)
ctrl = trainControl(summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
set.seed(476)
lrReduced = train(training[,reducedSet],
                  y = training$Class,
                  method = 'glm',
                  metric = 'ROC',
                  trControl = ctrl)
confusionMatrix(data = lrReduced$pred$pred, reference = lrReduced$pred$obs)

library(pROC)

reducedROC <- roc(response = lrReduced$pred$obs,
                  predictor = lrReduced$pred$successful,
                  levels = rev(levels(lrReduced$pred$obs)))

plot(reducedROC, legacy.axes = T)
auc(reducedROC)

varImp(lrReduced) #важность факторов

# дискриминантный анализ 
# Linear Discriminant Analysis

library(MASS)
set.seed(476)
IdaFit <- train(x = training[, reducedSet],
                y = training$Class,
                method = "lda",
                preProc = c("center","scale"),
                metric = 'ROC',
                trControl = ctrl)
ldaFit <- IdaFit
ldaFit

ldaTestClasses <- predict(IdaFit, newdata = testing[,reducedSet])
ldaTestProbs <- predict(IdaFit, newdata = testing[,reducedSet], type = 'prob')

plot(testing$Class, ldaTestClasses)
confusionMatrix(data = ldaTestClasses, reference = testing$Class)

library(pROC)
#redROC2
reducedROC <- roc(response = testing$Class,
                  predictor = ldaTestProbs$successful,
                  levels = rev(levels(testing$Class)))

plot(reducedROC, legacy.axes = T)
auc(reducedROC)

#partialLeastSquaresDiscrAnalysis

install.packages('pls')
set.seed(476)
plsFit <- train(x = training[, reducedSet],
                y = training$Class,
                method = "pls",
                tuneGrid = expand.grid(.ncomp = 1:10),
                preProc = c('center', 'scale'),
                metric = 'ROC',
                trControl = ctrl)

plsTestClasses <- predict(plsFit, newdata = testing[,reducedSet])
plsTestProbs <- predict(plsFit, newdata = testing[,reducedSet], type = 'prob')

plot(testing$Class, plsTestClasses)
confusionMatrix(data = plsTestClasses, reference = testing$Class)

library(pROC)
#redROC2
ROC3 <- roc(response = testing$Class,
                  predictor = plsTestProbs$successful,
                  levels = rev(levels(testing$Class)))

plot(ROC3, legacy.axes = T)
auc(reducedROC)
auc(ROC3)

#сравним
varImp(plsFit)
varImp(ldaFit)

#WS 11

load('training.RData')
load('testing.RData')
load('fullSet.RData')
load('reducedSet.RData')
library(caret)
library()
ctrl <- trainControl(summaryFunction = twoClassSummary, 
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(476)
#mdaFit 
  
load('mdaFit.RData')
install.packages('mda')
library(mda)

#mdaFit
predictedProbMDA <- predict(mdaFit, testing[,reducedSet], type = "prob")
predictedClassMDA <- predict(mdaFit, testing[,reducedSet])
  head(predictedProbMDA)
  predictedClassMDA
#Матрица
plot(testing$Class, predictedClassMDA)
confusionMatrix(data = predictedClassMDA, reference = testing$Class)

    library(pROC)
#ROC картинка  
  ROC <- roc(response = testing$Class,
              predictor = predictedProbMDA$successful,
              levels = rev(levels(testing$Class)))
  
  plot(ROC, legacy.axes = T)
  auc(ROC)

#nnetFit 
  
  load('nnetFit.RData')
  install.packages('mda')
  library(mda)
  
  #mdaFit
  predictedProbNN <- predict(nnetFit, testing[,reducedSet], type = "prob")
  predictedClassNN <- predict(nnetFit, testing[,reducedSet])
  head(predictedProbNN)
  #predictedClassMDA
  #Матрица
  plot(testing$Class, predictedClassNN)
  confusionMatrix(data = predictedClassNN, reference = testing$Class)
  
  library(pROC)
#ROC картинка  
  ROC <- roc(response = testing$Class,
             predictor = predictedProbNN$successful,
             levels = rev(levels(testing$Class)))
  
  plot(ROC, legacy.axes = T)
  auc(ROC)

#SVMFit 
  
  load('svmRModel.RData')
  #install.packages('mda')
  #library(mda)
  library(kernlab)
  #SVM
  predictedProbSVM <- predict(svmRModel, testing[,reducedSet], type = "prob")
  predictedClassSVM <- predict(svmRModel, testing[,reducedSet])
  head(predictedProbNN)
  #predictedClassSVM
  #Матрица
  plot(testing$Class, predictedClassNN)
  confusionMatrix(data = predictedClassSVM, reference = testing$Class)$Accuracy
  
  library(pROC)
  #ROC картинка  
  ROC <- roc(response = testing$Class,
             predictor = predictedProbSVM$successful,
             levels = rev(levels(testing$Class)))
  
  plot(ROC, legacy.axes = T)
  auc(ROC)  
  
#NaiveBayes
  load('nbPredictors.RData')
  load('nbTesting.RData')
  load('nbTraining.RData')

load('nBayesFit.RData')
install.packages('klaR')
library(klaR)
predictedProbNB <- predict(nBayesFit, nbTesting[,nbPredictors], type = "prob")
predictedClassNB <- predict(nBayesFit, nbTesting[,nbPredictors])
head(nbTesting)
#predictedClassSVM
#Матрица
plot(nbTesting$Class, predictedClassNB)
confusionMatrix(data = predictedClassNB, reference = nbTesting$Class)

library(pROC)
#ROC картинка  
ROC <- roc(response = nbTesting$Class,
           predictor = predictedProbNB$successful,
           levels = rev(levels(nbTesting$Class)))

plot(ROC, legacy.axes = T)
auc(ROC) 
