#Práctica III del perceptrón multicapa
#Clasificación multi etiqueta con el paquete neuralnet 

library(neuralnet)
library(nnet)
library(ggplot2)

# ----> Cargando datos
set.seed(10)
wines<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Técnicas de Inteligencia Artificial\\Actividades\\15 wines.csv")

names(wines) <- c("label",
                  "Alcohol", 
                  "Malic_acid", 
                  "Ash", 
                  "Alcalinity_of_ash", 
                  "Magnesium", 
                  "Total_phenols", 
                  "Flavanoids", 
                  "Nonflavanoid_phenols", 
                  "Proanthocyanins", 
                  "Color_intensity", 
                  "Hue", 
                  "OD280_OD315_of_diluted_wines", 
                  "Proline") 
head(wines) 

plt1 <- ggplot(wines, aes(x = Alcohol, y = Magnesium, colour = as.factor(label))) +
  geom_point(size=3) + 
  ggtitle("Wines") 

plt2 <- ggplot(wines, aes(x = Alcohol, y = Proline, colour = as.factor(label))) + 
  geom_point(size=3) + 
  ggtitle("Wines") 

plt1 
plt2


# ----> Procesamiento

#Codificar la variable categóricas objetivo como one-hot
# Encode as a one hot vector multilabel data 
train <- cbind(wines[, 2:14], class.ind(as.factor(wines$label))) 

# Set labels name 
names(train) <- c(names(wines)[2:14],"l1","l2","l3") 

#Estandarizar los predictores
#Scale data 
scl <- function(x){ (x - min(x))/(max(x) - min(x)) } 
train[, 1:13] <- data.frame(lapply(train[, 1:13], scl)) 
head(train)


# ----> Ajustar el modelo (neuralnet)
# Set up formula 
n <- names(train) 
f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1","l2","l3")], collapse = " + "))) 
f 

nn <- neuralnet(f,
                data = train, 
                hidden = c(13, 10, 3), 
                act.fct = "logistic", 
                linear.output = FALSE, 
                lifesign = "minimal") 

plot(nn) 

# Compute predictions 
pr.nn <- compute(nn, train[, 1:13]) 

# Extract results 
pr.nn_ <- pr.nn$net.result 
head(pr.nn_) 

# Accuracy (training set) 
original_values <- max.col(train[, 14:16]) 
pr.nn_2 <- max.col(pr.nn_) 
mean(pr.nn_2 == original_values) 

#Validación cruzada
set.seed(10) 
k <- 10 
outs <- NULL 
proportion <- 0.995 

library(plyr)  
pbar <- create_progress_bar('text') 
pbar$init(k) 

for(i in 1:k){
  index <- sample(1:nrow(train), round(proportion*nrow(train))) 
  train_cv <- train[index, ] 
  test_cv <- train[-index, ] 
  nn_cv <- neuralnet(f, 
  data = train_cv, 
  hidden = c(13, 10, 3), 
  act.fct = "logistic", 
  linear.output = FALSE) 
  # Compute predictions 
  pr.nn <- compute(nn_cv, test_cv[, 1:13]) 
  # Extract results 
  pr.nn_ <- pr.nn$net.result 
  # Accuracy (test set) 
  original_values <- max.col(test_cv[, 14:16]) 
  pr.nn_2 <- max.col(pr.nn_) 
  outs[i] <- mean(pr.nn_2 == original_values) 
  pbar$step() 
}

mean(outs) 


# ==== Comparar con Random Forest ====
library(randomForest)
library(caret)
library(dplyr)

str(wines)

#Crear splits
set.seed(123)
index <- createDataPartition(wines$label, p=0.80, list=FALSE)
train <- wines[index,] 
test <- wines[-index,] 

train$label <- as.factor(train$label)
test$label <- as.factor(test$label)

#Entrenar el modelo
rForest <- randomForest(label ~ ., data=train, maxnodes = 10, ntree = 20) 
print(rForest)
#print(rForest$confusion)
plot(rForest)

library(vcd)
kappa(rForest$confusion)




#xd