#Aprendizaje profundo utilizando redes neuronales multicapa 

# ==== Redes neuronales multicapa con neuralnet ====
library("neuralnet") 
library(ISLR) 

data = College 
str(data) 

#Escalado min-max
max_data <- apply(data[,2:18], 2, max)  
min_data <- apply(data[,2:18], 2, min) 
data_scaled <- scale(data[,2:18],center = min_data, scale = max_data - min_data)  

#Convertir a num. variable categórica
Private = as.numeric(College$Private)-1 
data_scaled = cbind(Private,data_scaled) 

index = sample(1:nrow(data),round(0.70*nrow(data))) 
train_data <- as.data.frame(data_scaled[index,]) 
test_data <- as.data.frame(data_scaled[-index,]) 

#Entrenar la red neuronal multicapa
n = names(train_data) 
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))  # creamos la fórmula que usaremos para construir la red
deep_net = neuralnet(f,data=train_data,hidden=c(5,3),linear.output=F) 

plot(deep_net) 

predicted_data <- compute(deep_net,test_data[,2:18]) 
print(head(predicted_data$net.result)) 

predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0) #Redondear a 0 o 1
table(test_data$Private,predicted_data$net.result) 

table(test_data$Private)
51+8
table(predicted_data$net.result) #obtener las ocurrencias en los datos predichos
51+8
8+166

#Precisión de la matriz de confusión
Acc = (51 + 166)/(51 + 166 + 8 + 8) 
Acc 


# ==== Entrenamiento y modelado de una DNN con H2O ====
library(h2o) 

h2o.init()

#inicia el motor h2o | memoria máxima de 2 GB y dos núcleos paralelos
c1=h2o.init(max_mem_size = "2G",
            nthreads = 2, 
            ip = "localhost", 
            port = 54321) 

data(iris) 
summary(iris) 
iris_d1 <- h2o.deeplearning(1:4,5,
                            as.h2o(iris),hidden=c(5,5),
                            export_weights_and_biases=T)

iris_d1 
plot(iris_d1) 

h2o.weights(iris_d1, matrix_id=1) 
h2o.weights(iris_d1, matrix_id=2) 
h2o.weights(iris_d1, matrix_id=3) 

h2o.biases(iris_d1, vector_id=1) 
h2o.biases(iris_d1, vector_id=2) 
h2o.biases(iris_d1, vector_id=3) 

# plot weights connecting `Sepal.Length` to first hidden neurons 
plot(as.data.frame(h2o.weights(iris_d1, matrix_id=1))[,1]) 



















#xd