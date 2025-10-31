# Algunos ejemplos extra de RNA's

# ==== Uso de redes neuronales para comprimir y visualizar datos multidimensionales ====
library(neuralnet)
library(caret)
library(ggplot2)

# Imprime la grafica de pares simples 
featurePlot(x=iris[,1:4], y=iris$Species, plot="pairs", pch=21, auto.key=list(columns=3))

train<-iris
train<-train[,1:4]
train<-cbind(train,train)
colnames(train)<-c("SL.I","SW.I","PL.I","PW.I", #Inputs del dataset de iris
                   "SL.O","SW.O","PL.O","PW.O") #los mismos outputs

# Procesar previamente el conjunto de datos de entrenamiento con el paquete caret 
preProcValues<-preProcess(train, method = c("range"))
train2<-predict(preProcValues,train)

#Crear RNA que comprime los datos de origen 4D en un conjunto de datos 2D
set.seed(1)
model2d<-neuralnet(SL.O + SW.O + PL.O + PW.O ~ SL.I + SW.I + PL.I + PW.I,
                    train2, hidden=c(3,2,3), algorithm='rprop+', threshold=0.01)

print (paste("Mean square error = ", model2d$result.matrix[1])) 
plot(model2d, rep=1)

#3. obtenemos los valores de activación de la capa oculta de en medio para crear un nuevo conjunto de datos 2D comprimido
result2d <- compute(model2d, train2[,1:4]) # Run input set through the neural network
out2d <- as.data.frame(result2d$neurons[[3]][,2:3]) # Get the 2D-data from middle layer
out2d <- cbind(out2d, iris[,5]) # Append labels from source dataset
colnames(out2d) <- c("HL3.1","HL3.2", "SPECIES") 
ggplot(out2d, aes(x=HL3.1, y=HL3.2, color=SPECIES)) + geom_point() # Visualize it 


#¡Intentemos enviar los datos a través de una sola neurona! 
set.seed(1)
model1d<-neuralnet(SL.O + SW.O + PL.O + PW.O ~ SL.I + SW.I + PL.I + PW.I,
                   train2, hidden=c(3,1,3), algorithm = 'rprop+', threshold = 0.01)
print(paste("Mean square error = ", model1d$result.matrix[1]))
plot(model1d, rep=1)

result1d<-compute(model1d, train2[,1:4]) #Run them through the neural network
out1d<-as.data.frame(result1d$neurons[[3]][,2])
out1d<-cbind(out1d, iris[,5])
colnames(out1d)<-c("HL1", "SPECIES")
ggplot(out1d, aes(x=SPECIES, y=HL1, color=SPECIES)) +
  geom_violin(trim = FALSE) +
  geom_jitter(position = position_jitter(0.2))



# ==== RNAs para la predicción ====
library(MASS)

set.seed(500)
data<-Boston

apply(data,2,function(x) sum(is.na(x))) 

#Dividir aleatoriamente el dataset en 75:25
index <- sample(1:nrow(data),round(0.75*nrow(data))) 
train <- data[index,] 
test <- data[-index,] 
lm.fit <- glm(medv~., data=train) #Se construye un modelo de regresión lineal
summary(lm.fit)

pr.lm <- predict(lm.fit,test) 
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test) 
MSE.lm


# ----> Preparación para adaptarse a la red neuronal
#Normalizar los datos
maxs <- apply(data, 2, max)  
mins <- apply(data, 2, min) 
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins)) 
train_ <- scaled[index,] 
test_ <- scaled[-index,] 

#Parámetros 
library(neuralnet) 

n <- names(train_) 
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + "))) 
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T) 
plot(nn)


# ----> Predicciones
#Predecir: medv
pr.nn <- compute(nn,test_[,1:13]) 
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv) 
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv) 
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_) 

#Comparar los dos MSE
print(paste(MSE.lm,MSE.nn)) 

par(mfrow=c(1,2)) 
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7) 
abline(0,1,lwd=2) 
legend('bottomright',legend='NN',pch=18,col='red', bty='n') 
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7) 
abline(0,1,lwd=2) 
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7) 
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7) 
abline(0,1,lwd=2) 
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue')) 


# ----> Validación cruzada rápida (prueba de tren dividida)
library(boot)

set.seed(200)
lm.fit<-glm(medv ~ ., data=data)
cv.glm(data, lm.fit, K=10)$delta[1]

#Ahora la red. Ten en cuenta que estamos dividiendo los datos de esta manera: 90% conjunto de 
#entrenamiento y 10% de prueba establecidos de forma aleatoria por 10 veces

set.seed(450) 
cv.error <- NULL 
k <- 10 

library(progress)
library(plyr)
#pbar<-create_progress_bar('text')

pbar <- progress_bar$new(
  format = " [:bar] :percent :elapsed",
  total = k, clear = FALSE, width = 60)

for(i in 1:k){ 
  index <- sample(1:nrow(data),round(0.9*nrow(data))) 
  train.cv <- scaled[index,] 
  test.cv <- scaled[-index,] 
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T) 
  
  pr.nn <- compute(nn,test.cv[,1:13]) 
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv) 
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv) 
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv) 
  
  #pbar$step() 
  pbar$tick()
}


mean(cv.error)
cv.error
boxplot(cv.error, xlab='MSE CV', col='cyan', border='blue', names='CV error (MSE)',
        main='CV error (MSE) for NN', horizontal=TRUE)





#