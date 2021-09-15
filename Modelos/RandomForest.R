# Se importan las librerias
library(randomForest);
library(gmodels);

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
trainData <- read.csv("trainData.csv", sep=",");

# Se fija la semilla
set.seed(12);

# Se define la funcion de validacion cruzada
self.cv <- function(arboles, p=0.1){
  meanRMSE <- c();
  for(n in arboles){
    rmseList <- c();
    for(k in seq(1:10)){
      validationIndexList <- sample(nrow(trainData), nrow(trainData)*p);
      subTrainData <- trainData[-validationIndexList,];
      crossData <- trainData[validationIndexList,];
      rfModel <- randomForest(Coliformes ~ ., data = subTrainData, ntree = n);
      yhat <- predict(rfModel, crossData);
      rmse <- sqrt(mean((yhat - crossData$Coliformes)^2));
      rmseList <- append(rmseList, rmse);
    }
    meanRMSE <- append(meanRMSE, mean(rmseList));
  }
  return(meanRMSE);
} 

arboles <- c(10, 50, 100, 500, 1000);
crossResult <- self.cv(arboles);

plot(y = crossResult, x = arboles, type="b",  pch=16, xlab="", ylab="");
mtext(side=1, line=3, "Cantidad de Árboles", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "RMSE promedio", col="black", font=2,cex=1.2);

# Se construye el mejor modelo
# ---------------------------
rfBest <- randomForest(Coliformes ~ ., data = trainData, ntree = 50);
plot(y=sqrt(rfBest$mse), x=seq(1,50), type="l", xlab="", ylab="");
mtext(side=1, line=3, "Cantidad de Árboles", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "RMSE", col="black", font=2,cex=1.2);

# Importancia de las variables
variables <- importance(rfBest, scale = F);
dotchart(sort(variables[,1]), xlab="", pch=16)
mtext(side=1, line=3, "Índice Gini", col="black", font=2,cex=1.2);

# Se obtiene el conjunto de datos de prueba
testData <- read.csv("testData.csv", sep=",");

# Prediccion
rfPred <- predict(rfBest, testData);

# Metricas
rfMSE <- mean((rfPred-testData$Coliformes)^2);
rfRMSE <- sqrt(rfMSE);
rfRMLSE <- sqrt(mean((log(rfPred+1)-log(testData$Coliformes+1))^2));
rfMAE <- mean(abs(rfPred-testData$Coliformes));
rfMSE;
rfRMSE;
rfRMLSE;
rfMAE;

# Prueba de normalidad
shapiro.test(rfPred);

# Prueba de correlacion
cor.test(rfPred, testData$Coliformes, method="spearman");
cor.test(rfPred, testData$Coliformes, method="kendall");
