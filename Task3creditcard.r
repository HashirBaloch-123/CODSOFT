install.packages("ROSE")
library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(ROSE)
data <- read_csv("creditcard.csv")
data$Class <- as.factor(data$Class)
data[,1:30] <- scale(data[,1:30])
data_balanced <- ROSE(Class ~ ., data = data, seed = 1)$data
set.seed(123)
trainIndex <- createDataPartition(data_balanced$Class, p = .7, list = FALSE, times = 1)
trainData <- data_balanced[trainIndex,]
testData <- data_balanced[-trainIndex,]
rf_model <- randomForest(Class ~ ., data = trainData, ntree = 100)
rf_pred_class <- predict(rf_model, newdata = testData)
rf_conf_matrix <- confusionMatrix(rf_pred_class, testData$Class)
rf_precision <- rf_conf_matrix$byClass["Pos Pred Value"]
rf_recall <- rf_conf_matrix$byClass["Sensitivity"]
rf_F1 <- 2 * ((rf_precision * rf_recall) / (rf_precision + rf_recall))
cat("Random Forest - Precision:", rf_precision, "Recall:", rf_recall, "F1-Score:", rf_F1, "\n")

