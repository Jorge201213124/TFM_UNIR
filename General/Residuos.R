# Se importan las librerias
library(ggplot2);
library(gridExtra);

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
resultData <- read.csv("resultData.csv", sep=",");
difData <- data.frame(resultData$real-resultData$tree,
                      resultData$real-resultData$pruneTree,
                      resultData$real-resultData$rf,
                      resultData$real-resultData$svm,
                      resultData$real-resultData$nn,
                      resultData$real-resultData$dnn);
colnames(difData) <- c('dtree','dpruneTree','drf','dsvm','dnn','ddnn');

# Analisis grafico
par(mfrow=c(1,2));
plot(resultData$real, resultData$tree, main="Árbol completo", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);
plot(resultData$real, resultData$pruneTree, main="Árbol podado", pch=16,
     xlim = c(0, 1), ylim=c(0,1),
     xlab = "Valor real", ylab="Predicción");
abline(0,1);

# Estadistica descriptiva
summary(difData$dtree);
sd(difData$dtree);

summary(difData$dpruneTree);
sd(difData$dpruneTree);

summary(difData$drf);
sd(difData$drf);

summary(difData$dsvm);
sd(difData$dsvm);

summary(difData$dnn);
sd(difData$dnn);

summary(difData$ddnn);
sd(difData$ddnn);


# Analisis de distribucion
N <- nclass.Sturges(difData$dtree);

treePlot <- ggplot(difData, aes(x = dtree)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Árbol completo",x="Residuo", y = "Cantidad");
prunePlot <- ggplot(difData, aes(x = dpruneTree)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Árbol podado",x="Residuo", y = "Cantidad");
grid.arrange(treePlot, prunePlot, ncol=2, nrow = 1);

rfPlot <- ggplot(difData, aes(x = drf)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Bosque aleatorio",x="Residuo", y = "Cantidad");
svmPlot <- ggplot(difData, aes(x = dsvm)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Maquina de vectores",x="Residuo", y = "Cantidad");
grid.arrange(rfPlot, svmPlot, ncol=2, nrow = 1);

nnPlot <- ggplot(difData, aes(x = dnn)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Red neuronal",x="Residuo", y = "Cantidad");
dnnPlot <- ggplot(difData, aes(x = ddnn)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  labs(title="Red profunda",x="Residuo", y = "Cantidad");
grid.arrange(nnPlot, dnnPlot, ncol=2, nrow = 1);


# Independencia de residuos
par(mfrow=c(1,2));
acf(difData$dtree, xlab='Retraso', main= "Árbol completo");
acf(difData$dtree, xlab='Retraso', main= "Árbol podado");

par(mfrow=c(1,2));
acf(difData$drf, xlab='Retraso', main= "Bosque aleatorio");
acf(difData$dsvm, xlab='Retraso', main= "Maquina de vectores");

par(mfrow=c(1,2));
acf(difData$dnn, xlab='Retraso', main= "Red neuronal");
acf(difData$ddnn, xlab='Retraso', main= "Red profunda");
