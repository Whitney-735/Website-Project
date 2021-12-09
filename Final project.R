library(readr)
library(caret)
library(tidyverse)
library(GGally)
WineData <- read_csv("~/Desktop/ACC8143/WineData.csv")

head(WineData,10)
GGally::ggpairs(WineData, aes(color = Variety, alpha = 0.5))

ggplot(data=WineData,mapping = aes(x=`fixed acidity`,y=sulphates,color=Variety))+
  geom_point(alpha=0.5)

ggplot(WineData, aes(x = Variety, y = alcohol, color= Variety))+
  geom_boxplot()+ 
  ggtitle("Variety vs. Alcohol")

ggplot(WineData, aes(x = Variety, y = density, color= Variety ))+
  geom_boxplot()+ 
  ggtitle("Variety vs. Density")

summary(WineData)

### machine learning 

trainIndex <- createDataPartition(WineData$Variety, p = .6, list = FALSE, times = 1)

SVMTrain <- WineData[ trainIndex,]
SVMTest  <- WineData[-trainIndex,]

Wine_SVM <- train(
  form = factor(Variety) ~ .,
  data = SVMTrain,
  trControl = trainControl(method = "cv", number = 10,
                           classProbs =  TRUE),
  method = "svmLinear",
  preProcess = c("center", "scale"),
  tuneLength = 10)
Wine_SVM
summary(Wine_SVM)

svm_Pred<-predict(Wine_SVM,SVMTest,type="prob")

svmtestpred<-cbind(svm_Pred,SVMTest)


svmtestpred<-svmtestpred%>%
  mutate(prediction=if_else(Red>White,"Red", "White"))
table(svmtestpred$prediction)                            

confusionMatrix(factor(svmtestpred$prediction),factor(svmtestpred$Variety))
###
supportvectors<-SVMTrain[Wine_SVM$finalModel@SVindex,]

ggplot(data=SVMTest, mapping = aes(x=`fixed acidity`,y=sulphates,color=Variety))+
  geom_point(alpha=0.5)+
  geom_point(data=svmtestpred, mapping = aes(x=`fixed acidity`,y=sulphates, color=prediction),shape=6,size=3)+
  geom_point(data=supportvectors, mapping = aes(x=`fixed acidity`,y=sulphates),shape=4,size=4)+
  theme(legend.title = element_blank())+ggtitle("SVM Demonstration")




