#Ejemplos del perceptrón multicapa y redes de propagación hacia adelante

# ==== Propagación hacia adelante (forward propagation) o feed forward ====
# W0,0: 0.4
# W0,1: 0.1
# W0,2: -0.1
# W0,3: -0.1
# W1,0: 0.06
# W1,1: -0.4


beta <- 0.45 # tasa de aprendizaje
alpha <- 0.9 # impulso o momentum
input <- N0 <- matrix(c(1,1)) 
w0 <- matrix(c(.4,.1,-.1,-.1), nrow=2) 
print(input) 
print(w0) 

sigma <- function(t) 1/(1+exp(-t))

#Calcula las entradas en la capa oculta.
N1 <- sigma(w0 %*% input) 
print(N1) 

#Calcula las entradas en la capa de salida
w1 <- matrix(c(0.06, -0.4), nrow=1) 
print(w1) 
N2 <- sigma(w1 %*% N1) 
N2

# - Retropropagación -
N2.0.error <- N2 * (1-N2) * (1-N2) 
print(N2.0.error)

#Calcula la tasa de cambio de los pesos en cada uno de los dos nodos con la ecuación
w1.Rate = (beta * N2.0.error[1,1]) * t(N1) 
print(w1.Rate)
print(w1) 

#Calcula los nuevos pesos
t <- 1 
w1.new <- w1 + w1.Rate + alpha*(t-1) 
print(w1.new)

N1.0.error <- N2.0.error %*% w1.new 

#Calcula la tasa de cambio de los pesos entre la entrada y la capa oculta
w0.Rate = t(beta * N1.0.error) * (N0) 
print(w0.Rate) 


w0.Rate <- matrix(c(w0.Rate[1,1], w0.Rate[2,1], w0.Rate[1,1], w0.Rate[2,1]), nrow=2) 
print(w0.Rate) 

#Calcula los nuevos pesos en la capa de entrada. 
w0.new <- w0 + w0.Rate + alpha*(t-1) 
print(w0.new) 


# -- Segunda iteración --
#Se ejecuta la propagación hacia adelante nuevamente para ver si hay mejoras
w0 <- w0.new 
N1 <- sigma(w0 %*% input) 
w1 <- w1.new 
N2 <- sigma(w1 %*% N1) 
print(N2) 

N2.1.error <- N2 * (1-N2) * (1-N2) 
print(N2.1.error) 



# ----> Modificando los valores de los pesos
# W0,0: 0.2
# W0,1: 0.4
# W0,2: -0.3
# W0,3: -0.6
# W1,0: 0.007
# W1,1: -0.05


beta <- 0.45 # tasa de aprendizaje
alpha <- 0.9 # impulso o momentum
input <- N0 <- matrix(c(1,1)) 
w0 <- matrix(c(.2,.4,-.3,-.6), nrow=2) 
print(input) 
print(w0) 

sigma <- function(t) 1/(1+exp(-t))

#Calcula las entradas en la capa oculta.
N1 <- sigma(w0 %*% input) 
print(N1) 

#Calcula las entradas en la capa de salida
w1 <- matrix(c(0.07, -0.05), nrow=1) 
print(w1) 
N2 <- sigma(w1 %*% N1) 
N2

# - Retropropagación -
N2.0.error <- N2 * (1-N2) * (1-N2) 
print(N2.0.error)

#Calcula la tasa de cambio de los pesos en cada uno de los dos nodos con la ecuación
w1.Rate = (beta * N2.0.error[1,1]) * t(N1) 
print(w1.Rate)
print(w1) 

#Calcula los nuevos pesos
t <- 1 
w1.new <- w1 + w1.Rate + alpha*(t-1) 
print(w1.new)

N1.0.error <- N2.0.error %*% w1.new 

#Calcula la tasa de cambio de los pesos entre la entrada y la capa oculta
w0.Rate = t(beta * N1.0.error) * (N0) 
print(w0.Rate) 


w0.Rate <- matrix(c(w0.Rate[1,1], w0.Rate[2,1], w0.Rate[1,1], w0.Rate[2,1]), nrow=2) 
print(w0.Rate) 

#Calcula los nuevos pesos en la capa de entrada. 
w0.new <- w0 + w0.Rate + alpha*(t-1) 
print(w0.new) 


# -- Segunda iteración --
#Se ejecuta la propagación hacia adelante nuevamente para ver si hay mejoras
w0 <- w0.new 
N1 <- sigma(w0 %*% input) 
w1 <- w1.new 
N2 <- sigma(w1 %*% N1) 
print(N2) 

N2.1.error <- N2 * (1-N2) * (1-N2) 
print(N2.1.error) 

print(N2.0.error)


# ==== Perceptrón multicapa  ====
aggregate(cbind(n.cases = Sepal.Length) ~ Species, iris, length)

set.seed(18121842) 
iris.setosa <- iris[iris$Species == 'setosa',] 
iris.versicolor <- iris[iris$Species == 'versicolor',] 
iris.virginica <- iris[iris$Species == 'virginica',]

split_data<-function(N,p){
  stopifnot(p > 0 & p < 1)
  n<-N*p
  trn.index<-sample.int(N,n,replace = FALSE)
  test.index<-setdiff(1:N,trn.index)
  list(train=trn.index,test=test.index)
}

setosa.split <- split_data(nrow(iris.setosa), 0.2) 
setosa.train <- iris.setosa[setosa.split[["train"]],] 
setosa.test  <- iris.setosa[setosa.split[["test"]],] 

versicolor.split <- split_data(nrow(iris.versicolor), 0.2) 
versicolor.train <- iris.versicolor[versicolor.split[["train"]],] 
versicolor.test  <- iris.versicolor[versicolor.split[["test"]],] 

virginica.split <- split_data(nrow(iris.virginica), 0.2) 
virginica.train <- iris.virginica[virginica.split[["train"]],] 
virginica.test  <- iris.virginica[virginica.split[["test"]],] 

train.data <- rbind(setosa.train, versicolor.train, virginica.train) 
test.data  <- rbind(setosa.test, versicolor.test, virginica.test) 

rm(
  setosa.split,
  versicolor.split,
  virginica.split,
  iris.setosa,
  iris.versicolor,
  iris.virginica
)

rm(setosa.train, versicolor.train, virginica.train)
rm(setosa.test, versicolor.test, virginica.test)

#Entrenando la red - 2
library(nnet)

nn.1<-nnet(Species ~ ., data=train.data, size=2,
           decay=1e-5,
           maxit=50)

summary(nn.1)

#Probar la red con predicciones
predicted.species<-predict(nn.1, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted=predicted.species)
table(comparison)

#Modelo con aumento del número de iteraciones
nn.2<-nnet(Species ~ ., data=train.data, size=2,
           decay=1e-5,
           maxit=100)

predicted.species<-predict(nn.2, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted=predicted.species)
table(comparison)
summary(nn.2)

#Modelo con más unidades en la capa oculta
nn.3<-nnet(Species ~ ., data=train.data, size=4,
           decay=1e-5,
           maxit=50)

predicted.species<-predict(nn.3, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted.species)
table(comparison)

#Modelo con más unidades y más iteraciones
nn.4<-nnet(Species ~ ., data=train.data, size=4,
           decay=1e-5,
           maxit=50)

predicted.species<-predict(nn.4, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted.species)
table(comparison)

#Modelo con menos funciones
nn.5<-nnet(Species ~ Petal.Length + Petal.Width, data=train.data,
           size=4, decay=1e-5, maxit=50)

predicted.species<-predict(nn.5, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted=predicted.species)
table(comparison)

#Modelo con menos características y más iteraciones
nn.6<-nnet(Species ~ Petal.Length + Petal.Width, data=train.data,
           size=4, decay=1e-5, maxit=50)

predicted.species<-predict(nn.6, test.data, type="class")
comparison<-data.frame(actual=test.data$Species, predicted=predicted.species)
table(comparison)

summary(nn.6)


# ----> NN.2 vs NN.6
# Modelo nn.2 – Red 4-2-3
pesos_nn2 <- data.frame(
  Conexion = c("sesgo", "h1", "h2"),
  setosa = c(19.92, -52.35, 25.18),
  versicolor = c(12.96, 0.67, -8.92),
  virginica = c(-31.85, 51.84, -17.48))

pesos_nn2

# Modelo nn.6 – Red 2-4-3
pesos_nn6 <- data.frame(
  Conexion = c("sesgo", "h1", "h2", "h3", "h4"),
  setosa = c(6.05, 28.12, -1.50, -33.99, 24.82),
  versicolor = c(0.25, -3.76, 4.62, 2.77, 12.57),
  virginica = c(-6.21, -24.85, -3.16, 31.54, -38.13))

pesos_nn6




#xd