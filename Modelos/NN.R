# Se importan las librerias
library(neuralnet);
library(NeuralNetTools);

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
trainData <- read.csv("trainData.csv", sep=",");

# Se definen las funciones de activacion

self.cv <- function(arq, actFun, method, alpha){
  print(c(arq, actFun, method, alpha));
  rmseList <- c();
  for(k in seq(1:10)){
    validationIndexList <- sample(nrow(trainData), nrow(trainData)*0.1);
    subTrainData <- trainData[-validationIndexList,];
    crossData <- trainData[validationIndexList,];
    nnModel <- neuralnet(Coliformes ~ .,
                         data = subTrainData, hidden = arq,
                         act.fct = actFun,
                         algorithm = method, 
                         learningrate = alpha
                         );
    if(length(nnModel$weights) > 0){
      yhat <- predict(nnModel, crossData);
      rmse <- sqrt(mean((yhat - crossData$Coliformes)^2));
      rmseList <- append(rmseList, rmse); 
    }
  }
  
  return(mean(na.omit(rmseList)));
}

# Arquitectura 1
set.seed(43);
self.cv(c(2,1), 'tanh', 'rprop+', NULL);
self.cv(c(2,1), 'logistic', 'rprop+', NULL);
self.cv(c(2,1), 'tanh', 'sag', NULL);
self.cv(c(2,1), 'logistic', 'sag', NULL);
self.cv(c(2,1), 'tanh', 'backprop', 0.001);
self.cv(c(2,1), 'logistic', 'backprop', 0.001);

# Arquitectura 2
set.seed(38);
self.cv(c(3,2), 'tanh', 'rprop+', NULL);
self.cv(c(3,2), 'logistic', 'rprop+', NULL);
self.cv(c(3,2), 'tanh', 'sag', NULL);
self.cv(c(3,2), 'logistic', 'sag', NULL);
self.cv(c(3,2), 'tanh', 'backprop', 0.001);
self.cv(c(3,2), 'logistic', 'backprop', 0.001);

# Arquitectura 3
set.seed(9);
self.cv(c(4,2), 'tanh', 'rprop+', NULL);
self.cv(c(4,2), 'logistic', 'rprop+', NULL);
self.cv(c(4,2), 'tanh', 'sag', NULL);
self.cv(c(4,2), 'logistic', 'sag', NULL);
self.cv(c(4,2), 'tanh', 'backprop', 0.001);
self.cv(c(4,2), 'logistic', 'backprop', 0.001);

# Arquitectura 4
set.seed(61);
self.cv(c(3,2,2), 'tanh', 'rprop+', NULL);
self.cv(c(3,2,2), 'logistic', 'rprop+', NULL);
self.cv(c(3,2,2), 'tanh', 'sag', NULL);
self.cv(c(3,2,2), 'logistic', 'sag', NULL);
self.cv(c(3,2,2), 'tanh', 'backprop', 0.001);
self.cv(c(3,2,2), 'logistic', 'backprop', 0.001);


set.seed(1);
self.cv(c(16,8,4,2,1), 'logistic', 'rprop+', NULL);
set.seed(1);
self.cv(c(2,1), 'logistic', 'sag', NULL);
set.seed(1);
self.cv(c(4,2,2,1), 'logistic', 'sag', NULL);


# Se entrena el mejor modelo
set.seed(123);
finalModel <- neuralnet(Coliformes ~ .,
                     data = trainData, hidden = c(2,1),
                     act.fct = 'tanh',
                     algorithm = 'sag',
                     rep=11
);

# Se grafica el modelo
plot(finalModel);

# Se obtiene el conjunto de datos de prueba
testData <- read.csv("testData.csv", sep=",");
yhat <- predict(finalModel, testData);
plot(yhat, testData$Coliformes);
abline(0,1);

# Metricas
nnMSE <- mean((yhat - testData$Coliformes)^2);
nnRMSE <- sqrt(nnMSE);
nnRMLSE <- sqrt(mean((log(yhat+1)-log(testData$Coliformes+1))^2));
nnMAE <- mean(abs(yhat-testData$Coliformes));
nnMSE;
nnRMSE;
nnRMLSE;
nnMAE;

# Prueba de normalidad
shapiro.test(yhat);

# Prueba de correlacion
cor.test(yhat, testData$Coliformes, method="spearman");
cor.test(yhat, testData$Coliformes, method="kendall");


# Datos red profunda
deepData <- read.csv("nnProf.csv", sep=",");

# Prueba de normalidad
shapiro.test(deepData$pred);


# Prueba de correlacion
cor.test(deepData$pred, deepData$real, method="spearman");
cor.test(deepData$pred, deepData$real, method="kendall");


# Grafica de puntos
par(mfrow=c(1,2));
plot(testData$Coliformes, yhat, main="Red neuronal", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
plot(deepData$real, deepData$pred, main="Red profunda", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);



# Guardar archivo
#resultDf <- data.frame(testData$Coliformes, yhatFull, 
                       pruneYhat, rfPred, yhatSvm, yhat, deepData$pred);
#colnames(resultDf) <- c('real','tree','pruneTree','rf','svm','nn','dnn');

#write.csv(resultDf,"resultData.csv", row.names = F);

