#Práctica II del perceptrón multicapa

# ==== Ejemplos de problemas resueltos con RNA ====

# ----> Ejemplo 1
library(neuralnet) 

df <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Técnicas de Inteligencia Artificial\\Actividades\\14 TinyData.csv") 
df$Like <- df$Acceptance=="like" 
df$Dislike <- df$Acceptance=="dislike" 


set.seed(1) 
nn <- neuralnet(Like + Dislike ~ Salt.Score + Fat.Score, data = df, linear.output = F, hidden = 3) 

nn$weights # display weights 

# display predictions 
prediction(nn)

# plot network 
plot(nn, rep="best") 


# ----> Ejemplo 2
library(MASS) # Necesario para muestrear distribuciones gaussianas multivariadas 
library(neuralnet)

set.seed(4400)  
cov <- matrix(c(0.05, 0, 0, 0.05), 2, 2) # Diagonal covariance matrix 
cov 

num.points <- 5000  
first <- mvrnorm(num.points, c(0, 0), cov) 
second <- mvrnorm(num.points, c(0, 1), cov) 
third <- mvrnorm(num.points, c(1, 0), cov) 
fourth <- mvrnorm(num.points, c(1, 1), cov) 
all.points <- rbind(first, second, third, fourth) 
labels <- rep(c(0, 1, 1, 0), each = num.points) 
xor.data <- as.data.frame(cbind(labels, all.points)) 
colnames(xor.data) <- c("label", "x", "y") 

num.sample.rows <- 10 
display.rows <- sample(1:nrow(xor.data), num.sample.rows) 
xor.data[display.rows, ] 

library(ggplot2) 
ggplot(xor.data, aes(x = x, y = y, color = factor(label))) + geom_point() + 
  scale_color_manual(name = "Labels", values = c("blue", "orange"),
                     labels = c("False", "True")) + 
  ggtitle("XOR Function Data") + xlab("X") + ylab("Y") 


xor.nnet <- neuralnet("label ~ x + y", data = xor.data, threshold = 1,
                      hidden = c(20), # 1 hidden layer with 20 units 
                      linear.output = F, # Classification 
                      err.fct = "ce", #Error Function 
                      act.fct = "logistic") #Activation Function 
cat(sprintf("Best error reached: %f", xor.nnet$result.matrix[c('error'), ])) 

test.data <- data.frame(x = c(0, 0, 1, 1),
                        y = c(0, 1, 0, 1), 
                        true.label = c(0, 1, 1, 0)) 
prediction <- compute(xor.nnet, test.data[, c("x", "y")])$net.result 
cbind(test.data, prediction) 

more.test.data <- data.frame(x = runif(10), y = runif(10)) 
more.predictions <- compute(xor.nnet, more.test.data)$net.result 
cbind(more.test.data, more.predictions) 


num.interpolating.points <- 100 
x.values <- seq(0, 1, len = num.interpolating.points) 
y.values <- seq(0, 1, len = num.interpolating.points) 
test.points <- as.data.frame(expand.grid(x.values, y.values)) 
colnames(test.points) <- c("x", "y") 
predictions <- compute(xor.nnet, test.points)$net.result 

ggplot() + geom_point(aes(x = test.points$x, y = test.points$y, color = predictions)) + 
  scale_color_gradient("Prediction", low = "blue", high = "orange") + 
  ggtitle("Visualizing the Neural Network's Decision Pattern") + xlab("X") + ylab("Y") 


library(devtools) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(xor.nnet, pos.col = "blue", neg.col = "orange")

source_gist('6206737')
gar.fun("y", xor.nnet) 

xor.2.nnet <- neuralnet("label ~ x + y", data = xor.data, threshold = 1,
                        hidden = c(20, 20), 
                        linear.output = F, 
                        err.fct = "ce", 
                        act.fct = "logistic") #Activation Function 
cat(sprintf("Best error reached: %f", xor.2.nnet$result.matrix[c('error'), ])) 

simple.predictions.2 <- compute(xor.2.nnet, test.data[, c("x", "y")])$net.result 
cbind(test.data, simple.predictions.2)

predictions.2 <- compute(xor.2.nnet, test.points)$net.result 
ggplot() + geom_point(aes(x = test.points$x, y = test.points$y, color = predictions.2)) +
  scale_color_gradient("Prediction", low = "blue", high = "orange") + 
  ggtitle("Visualizing the 2 Layer Neural Network's Decision Pattern") + xlab("X") + ylab("Y")

ggplot() + geom_point(aes(x = test.points$x, y = test.points$y, color = predictions.2 - predictions)) + 
  scale_color_gradient("Prediction", low = "blue", high = "orange") + 
  ggtitle("Visualizing the Neural Network's Decision Pattern") + xlab("X") + ylab("Y") 



# ----> Ejemplo 3
wbcd.url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data" 
wbcd.data <- read.csv(wbcd.url, header = F) 
wbcd.data <- wbcd.data[, -c(1)] 
wbcd.data[, 1] <- as.numeric(wbcd.data[, 1] == "M") 
colnames(wbcd.data)[1] <- "label" 
wbcd.data[1, ] 

train.proportion <- 0.8 
train.index <- sample(x = 1:nrow(wbcd.data),
                      size = floor(train.proportion * nrow(wbcd.data)), 
                      replace = F) 
#Dividir los datos en entrenamiento y prueba
wbcd.train.data <- wbcd.data[train.index, ] 
wbcd.test.data <- wbcd.data[-train.index, ] 
wbcd.test.labels <- wbcd.test.data$label 
wbcd.test.data <- subset(wbcd.test.data, select = -c(label)) 

formula <- sprintf("%s%s", "label ~ ", paste("V", 3:32, collapse = " + ", sep = "")) 
formula

wbcd.first.net <- neuralnet(formula, data = wbcd.train.data,
                            hidden = c(5), # 1 hidden layer with 5 units 
                            linear.output = F, rep = 5, 
                            err.fct = "ce", act.fct = "logistic", threshold = 2)

wbcd.second.net <- neuralnet(formula, data = wbcd.train.data, 
                             hidden = c(10), # 1 hidden layer with 10 units 
                             linear.output = F, rep = 5, 
                             err.fct = "ce", act.fct = "logistic", threshold = 2)

wbcd.third.net <- neuralnet(formula, data = wbcd.train.data, 
                            hidden = c(15), # 1 hidden layer with 15 units  
                            linear.output = F, rep = 5, 
                            err.fct = "ce", act.fct = "logistic", threshold = 2) 

wbcd.fourth.net <- neuralnet(formula, data = wbcd.train.data, 
                             hidden = c(5, 5), # 2 hidden layers with 5 units each  
                             linear.output = F, rep = 5, 
                             err.fct = "ce", act.fct = "logistic", threshold = 2)

wbcd.fifth.net <- neuralnet(formula, data = wbcd.train.data, 
                            hidden = c(10, 10), # 2 hidden layers with 10 units each 
                            linear.output = F, rep = 5, 
                            err.fct = "ce", act.fct = "logistic", threshold = 2) 

wbcd.sixth.net <- neuralnet(formula, data = wbcd.train.data, 
                            hidden = c(15, 15), # 2 hidden layers with 15 units each 
                            linear.output = F, rep = 5, 
                            err.fct = "ce", act.fct = "logistic", threshold = 2) 

train.scores <- sapply(list(wbcd.first.net, wbcd.second.net, wbcd.third.net,
                            wbcd.fourth.net, wbcd.fifth.net, wbcd.sixth.net), 
                       function(x) {min(x$result.matrix[c("error"), ])}) 

cat(paste(c(
  "Training Scores (Logarithmic Loss)\n1 Hidden Layer, 5 Hidden Units:",
  "1 Hidden Layer, 10 Hidden Units:",
  "1 + Hidden Layer, 15 Hidden Units:",
  "2 Hidden Layers, 5 Hidden Units Each:",
  "2 Hidden Layers, 10 Hidden Units Each:",
  "2 Hidden Layers, 15 Hidden Units Each:"
), train.scores, collapse = "\n"))

percentage.correctly.classified <- function(nn, threshold = 0.5) { 
  best <- which.min(nn$result.matrix[c("error"), ]) 
  net.predictions <- compute(nn, wbcd.test.data, rep = best)$net.result 
  thresholded.net.predictions <- ifelse(net.predictions > threshold, 1, 0) 
  num.correct <- sum(as.numeric(thresholded.net.predictions == wbcd.test.labels)) 
  num.correct / length(wbcd.test.labels) 
} 

scores <- sapply(list(wbcd.first.net, wbcd.second.net, wbcd.third.net,
                      wbcd.fourth.net, wbcd.fifth.net, wbcd.sixth.net),
                 percentage.correctly.classified)

cat(paste(c(
  "Test Scores (Percentage Correctly Classified)\n1 Hidden Layer, 5 Hidden Units:", 
  "1 Hidden Layer, +  10 Hidden Units:",
  "1 Hidden Layer, 15 Hidden Units:",
  "2 Hidden Layers, 5 Hidden Units Each:", 
  "2 Hidden Layers, +  10 Hidden Units Each:",
  "2 Hidden Layers, 15 Hidden Units Each:"), scores, collapse = "\n")) 



# ==== Entrenamiento y visualización de una red neuronal en R ====
library("neuralnet") 
library("ISLR") 

data = Auto 
View(data) 

# - Análisis exploratorio -
plot(data$weight, data$mpg, pch=data$origin,cex=2) 
par(mfrow=c(2,2)) 
plot(data$cylinders, data$mpg, pch=data$origin,cex=1) 
plot(data$displacement, data$mpg, pch=data$origin,cex=1) 
plot(data$horsepower, data$mpg, pch=data$origin,cex=1) 
plot(data$acceleration, data$mpg, pch=data$origin,cex=1) 

# ----> Modelo de red neuronal
mean_data <- apply(data[1:6], 2, mean) 
sd_data <- apply(data[1:6], 2, sd) 

mean_data
sd_data

# es una buena práctica normalizar los datos antes de entrenar una red neuronal
data_scaled <- as.data.frame(scale(data[,1:6],center = mean_data, scale = sd_data)) 
head(data_scaled, n=20) 

#Dividir los datos en 70:30
index = sample(1:nrow(data),round(0.70*nrow(data))) 
train_data <- as.data.frame(data_scaled[index,]) 
test_data <- as.data.frame(data_scaled[-index,]) 

n = names(data_scaled) 
f = as.formula(paste("mpg ~", paste(n[!n %in% "mpg"], collapse = " + "))) 

net = neuralnet(f,data=train_data,hidden=3,linear.output=TRUE)  #3 neuronas por capa
plot(net)
summary(net)
net$result.matrix #imprimir los pesos y sesgos

#Hacer predicciones con la red
predict_net_test <- compute(net,test_data[,2:6]) 
MSE.net <- sum((test_data$mpg - predict_net_test$net.result)^2)/nrow(test_data) 

#Comparar con un modelo de regresión lineal
Lm_Mod <- lm(mpg~., data=train_data) 
summary(Lm_Mod) 

predict_lm <- predict(Lm_Mod,test_data) 
MSE.lm <- sum((predict_lm - test_data$mpg)^2)/nrow(test_data) 

par(mfrow=c(1,2)) 
plot(test_data$mpg,predict_net_test$net.result,col='black',main='Real vs predicted for neural network',pch=18,cex=4) 
abline(0,1,lwd=5) 
plot(test_data$mpg,predict_lm,col='black',main='Real vs predicted for linear regression',pch=18,cex=4) 
abline(0,1,lwd=5)




#xd