# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los archivos csv
data2020 <- read.csv("Data2020.csv", sep=";");
data2019 <- read.csv("Data2019.csv", sep=";");
data2018 <- read.csv("Data2018.csv", sep=";");
data2017 <- read.csv("Data2017.csv", sep=";");
data2016 <- read.csv("Data2016.csv", sep=";");

# Integracion de datos
data <- rbind(data2020, data2019, data2018, data2017, data2016);

# Transformacion de datos
data$Temperatura[330] = min(data$Temperatura[data$Temperatura > 10]);

# Persistir informacion para analisis
write.csv(data,"Data.csv", row.names = F);

# Escalamiento de variables
# -------------------------
self.rescale <- function(data){
  nameList <- colnames(data);
  for(name in nameList){
    minColumn <- min(data[,name]);
    maxColumn <- max(data[,name]);
    rangeColumn <- maxColumn - minColumn;
    newColumn <- (data[,name] - minColumn)/rangeColumn;
    data[,name] <- newColumn;
  }
  return(data);
}

data$Coliformes <- log(data$Coliformes);
scaleData <- self.rescale(data);

# Dividir Datos
set.seed(123);
testIndexList <- sample(nrow(scaleData), 59);
testData <- scaleData[testIndexList,];
trainData <- scaleData[-testIndexList,];

# Desordenar los datos de entrenamiento
trainIndexList <- sample(nrow(trainData));
trainData <- trainData[trainIndexList, ];

# Persistir conjuntos
write.csv(testData,"testData.csv", row.names = F);
write.csv(trainData,"trainData.csv", row.names = F);


