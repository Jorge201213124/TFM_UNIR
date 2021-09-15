# Se importan las librerias
library(e1071);

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
trainData <- read.csv("trainData.csv", sep=",");

# Se definen los valores de hiperparametros
costList <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100);
gammaList <- c(0.01, 1/13, 0.1);  
degreeList <- c(2, 3, 4);
constList <- c(-0.5, 0, 0.5);

# Validacion cruzada kernel lineal
set.seed(12);
crossLinear <- tune(svm, Coliformes ~ ., data = trainData, 
                    tunecontrol = tune.control(sampling="cross", cross=10),
                    kernel= "linear",
                    ranges = list(cost = costList)
                    );
summary(crossLinear);

# Validacion cruzada kernel polinomial
set.seed(12);
crossPol <- tune(svm, Coliformes ~ ., data = trainData, 
                    tunecontrol = tune.control(sampling="cross", cross=10),
                    kernel= "polynomial",
                    ranges = list(cost = costList,
                                  gamma = gammaList,
                                  degree = degreeList,
                                  coef0 = constList
                    )
);
summary(crossPol);
tablaPol <- crossPol$performances;

# Validacion cruzada kernel radial
set.seed(12);
crossRad <- tune(svm, Coliformes ~ ., data = trainData, 
                 tunecontrol = tune.control(sampling="cross", cross=10),
                 kernel= "radial",
                 ranges = list(cost = costList,
                               gamma = gammaList
                 )
);
summary(crossRad);
tablaRad <- crossRad$performances;

# Validacion cruzada kernel sigmoide
set.seed(12);
crossSig <- tune(svm, Coliformes ~ ., data = trainData, 
                 tunecontrol = tune.control(sampling="cross", cross=10),
                 kernel= "sigmoid",
                 ranges = list(cost = costList,
                               gamma = gammaList,
                               coef0 = constList
                 )
);
summary(crossSig);
tablaSig <- crossSig$performances;

# Analisis de la validacion del kernel polinomial
set.seed(12);
crossPol2 <- tune(svm, Coliformes ~ ., data = trainData, 
                 tunecontrol = tune.control(sampling="cross", cross=10),
                 kernel= "polynomial",
                 coef0 = 0.5,
                 ranges = list(cost = costList,
                               gamma = gammaList
                 )
);

plot(crossPol2, type = 'contour', swapxy = T, transform.z = log2, nlevels = 8,
     color.palette = hsv_palette(h=0.15, v=1, from = 0.05, to=1),
     main = "", xlab="", ylab=""
);
mtext(side=1, line=3, "Gamma", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "Costo", col="black", font=2,cex=1.2);


summary(crossPol$best.model);
plot(crossPol$best.model$residuals);

# Se obtiene el conjunto de datos de prueba
testData <- read.csv("testData.csv", sep=",");

# Probando al mejor modelo
yhatSvm <- predict(crossPol$best.model, testData);
plot(yhatSvm, testData$Coliformes, main = 'Arbol Podado');
abline(0, 1);

# Metricas
svmMSE <- mean((yhatSvm-testData$Coliformes)^2);
svmRMSE <- sqrt(svmMSE);
svmRMLSE <- sqrt(mean((log(yhatSvm+1)-log(testData$Coliformes+1))^2));
svmMAE <- mean(abs(yhatSvm-testData$Coliformes));
svmMSE;
svmRMSE;
svmRMLSE;
svmMAE;

# Prueba de normalidad
shapiro.test(yhatSvm);

# Prueba de correlacion
cor.test(yhatSvm, testData$Coliformes, method="spearman");
cor.test(yhatSvm, testData$Coliformes, method="kendall");

# Grafica de puntos
par(mfrow=c(1,2));
plot(testData$Coliformes, rfPred, main="Bosque aleatorio", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
plot(testData$Coliformes, yhatSvm, main="Maquina de vectores", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
