# Se importan las librerias
library(tree);
library(MASS);
library(ggpubr)

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
trainData <- read.csv("trainData.csv", sep=",");

# Se fija la semilla
set.seed(12);

# Se construye el arbol
defaultTree <- tree(Coliformes ~ ., trainData);

# Se visualiza el arbol
summary(defaultTree);
plot(defaultTree);
text(defaultTree, pretty = 0);

# Se examina la cantidad de nodos
crossData <- cv.tree(defaultTree, K=10);
plot(crossData$size, crossData$dev, type = 'b', pch=16, 
     xlab="", ylab="");
mtext(side=1, line=3, "Cantidad de Nodos Terminales", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "Desviación", col="black", font=2,cex=1.2);
crossData$dev

# Se poda el arbol
pruneTree <- prune.tree(defaultTree, best = 4);
summary(pruneTree);
plot(pruneTree);
text(pruneTree, pretty = 0);

# Regiones de DBQO
plot(x=trainData$DBO5, y=trainData$Coliformes, xlab="", ylab="");
mtext(side=1, line=3, "DBOQ", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "Log Coliformes", col="black", font=2,cex=1.2);
polygon(x = c(0.005421, 7.5, 7.5, 0.005421), y = c(-1.2, -1.2, 20, 20), 
        col ="red", density=10);
polygon(x = c(0.005421, -3, -3, 0.005421), y = c(-1.2, -1.2, 20, 20), 
        col ="green", density=10, angle = 135);
abline(v=0.005421, col="blue");

# Regiones de DQO
plot(x=trainData$DQO, y=trainData$Coliformes, xlab="", ylab="");
mtext(side=1, line=3, "DQO", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "Log Coliformes", col="black", font=2,cex=1.2);
polygon(x = c(0.003478, 11, 11, 0.003478), y = c(-1.2, -1.2, 20, 20), 
        col ="red", density=10);
polygon(x = c(0.003478, -3, -3, 0.003478), y = c(-1.2, -1.2, 20, 20), 
        col ="green", density=8, angle = 135);
abline(v=0.003478, col="blue");

# Regiones de O2
plot(x=trainData$O2, y=trainData$Coliformes, xlab="", ylab="");
mtext(side=1, line=3, "O2", col="black", font=2,cex=1.2);
mtext(side=2, line=3, "Log Coliformes", col="black", font=2,cex=1.2);
polygon(x = c(0.5845, 11, 11, 0.5845), y = c(-1.2, -1.2, 20, 20), 
        col ="red", density=10);
polygon(x = c(0.5845, -3, -3, 0.5845), y = c(-1.2, -1.2, 20, 20), 
        col ="green", density=8, angle = 135);
abline(v=0.5845, col="blue");

# Se obtiene el conjunto de datos de prueba
testData <- read.csv("testData.csv", sep=",");

# Prediccion arbol sin podar
yhatFull <- predict(defaultTree, newdata = testData);

# Metricas
defaultMSE <- mean((yhatFull - testData$Coliformes)^2);
defaultRMSE <- sqrt(defaultMSE);
defaultRMSLE <- sqrt(mean((log(yhatFull+1)-log(testData$Coliformes+1))^2));
defaultMAE <- mean(abs(yhatFull-testData$Coliformes));
defaultMSE;
defaultRMSE;
defaultRMSLE;
defaultMAE;

# Prueba de Normalidad
shapiro.test(testData$Coliformes);
shapiro.test(yhatFull);

# Prueba de correlacion
cor.test(yhatFull, testData$Coliformes, method="spearman");
cor.test(yhatFull, testData$Coliformes, method="kendall");

# Prediccion arbol podado
pruneYhat <- predict(pruneTree, newdata = testData);
plot(pruneYhat, testData$Coliformes, main = 'Arbol Podado');
abline(0, 1);



pruneMSE <- mean((pruneYhat - testData$Coliformes)^2);
pruneRMSE <- sqrt(pruneMSE);
pruneRMSLE <- sqrt(mean((log(pruneYhat+1)-log(testData$Coliformes+1))^2));
pruneMAE <- mean(abs(pruneYhat-testData$Coliformes));
pruneMSE;
pruneRMSE;
pruneRMSLE;
pruneMAE;

# Prueba de normalidad
shapiro.test(pruneYhat);

# Prueba de correlacion
cor.test(pruneYhat, testData$Coliformes, method="spearman");
cor.test(pruneYhat, testData$Coliformes, method="kendall");

# Grafica de puntos
par(mfrow=c(1,2));
plot(testData$Coliformes, yhatFull, main="Arbol completo", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
plot(testData$Coliformes, pruneYhat, main="Arbol podado", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
