# Se importan las librerias
library(ggplot2);
library(gridExtra);

# Se define el directorio de trabajo
setwd("C:/Users/Jorge/Downloads/UnirTesis/Datos_Procesados");

# Se leen los datos
data <- read.csv("Data.csv", sep=",");

# Matriz de Correlacion
temp <- cor(data);

# Graficas de PUntos
ggplot(data, aes(x = Salinidad, y = Conductividad)) + geom_point() +
  xlab("Salinidad (%)") + ylab("Conductividad (uS/cm)");

ggplot(data, aes(x = Salinidad, y = Solidos.Totales)) + geom_point() + 
  xlab("Salinidad (%)") + ylab("Solidos Totales D.(mg/L)");

ggplot(data, aes(x = Salinidad, y = O2)) + geom_point() + 
  xlab("Salinidad (%)") + ylab("Oxígeno Disuelto (mg/L)");

ggplot(data, aes(x = O2, y = O2p)) + geom_point() + 
  xlab("Porcentaje Oxigeno Disuelto (%)") + ylab("Oxígeno Disuelto (mg/L)");

# Estadistica descriptiva
summary(data);

# Distribucion de Frecuencia
N <- nclass.Sturges(data$Caudal);

ggplot(data, aes(x = Caudal)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  xlab("Caudal (Its/seg)") + ylab("Cantidad");

# Generar distribucion normal
x2 <- seq(min(data$pH), max(data$pH), length = length(data$pH));
fn <- dnorm(x2, mean = mean(data$pH), sd = sd(data$pH));
ggplot(data, aes(x = pH)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  geom_line(aes(x = x2, y = fn*89)) +
  xlab("pH") + ylab("Cantidad");

# Generar distribucion normal
x2 <- seq(min(data$Temperatura), max(data$Temperatura), length = length(data$Temperatura));
fn <- dnorm(x2, mean = mean(data$Temperatura), sd = sd(data$Temperatura));
ggplot(data, aes(x = Temperatura)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  geom_line(aes(x = x2, y = fn*900)) +
  xlab("Temperatura (°C)") + ylab("Cantidad");

ggplot(data, aes(x = Conductividad)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  xlab("Conductividad (uS/cm)") + ylab("Cantidad");

ggplot(data, aes(x = O2)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  xlab("Oxigeno Disuelto (°C)") + ylab("Cantidad");

ggplot(data, aes(x = Coliformes)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  xlab("Coliformes Fecales (UFC/100 mL)") + ylab("Cantidad");

data$lColiformes <- log10(data$Coliformes);
ggplot(data, aes(x = lColiformes)) +
  geom_histogram(colour = 1, fill = "white",  bins = N) +
  xlab("Log Coliformes Fecales") + ylab("Cantidad");

