#Ejemplo de un perceptrón simple 

perceptron <-  function(X, y, random_seed = 553){
  # Esta función implementa el algoritmo del perceptron para encontrar un 
  # hiperplano que separe correctamente las dos clases. 
  # Transformar X en vectores aumentados 
  X <- cbind(1, X) 
  
  # Inicialización aleatoria del hiperplano 
  set.seed(random_seed) 
  w <- c(runif(n = 3, min = 0, max = 1)) 
  
  # Clasificación 
  clasificaciones <- predict_clase(X = X, w = w) 
  
  # Índice de las observaciones mal clasificadas 
  errores_clasificacion <- which((clasificaciones != y) == FALSE) 
  while (length(errores_clasificacion) > 0) { 
    # Se selecciona aleatoriamente una observación errónea 
    i <- sample(x = errores_clasificacion, size = 1) 
    
    # Actualización del hiperplano 
    w <- w + X[i,] * y[i] 
    clasificaciones <- predict_clase(X = X, w = w) 
    errores_clasificacion <- which((clasificaciones == y) == FALSE) 
  }
  return(w) 
} 

predict_clase <- function(X, w){
  # Esta función devuelve la clasificación de las observaciones
  # acorde al valor de sus predictores X y al hiperplano w 
  clase_predicha <- apply(X = X, MARGIN = 1, FUN = function(x){crossprod(x,w)}) 
  clase_predicha <- sign(clase_predicha) 
  return(clase_predicha) 
}

# Ejemplo observaciones linealmente separables en 2 dimensiones 
X <- matrix(c(8, 4, 9, 7, 9, 4, 10, 2, 8, 7, 4, 4, 1, 2, 7, 10, 7, 10, 6, 8, 10, 7, 3, 5, 4, 6, 3, 5), ncol = 2, byrow = FALSE) 
y <- c(1, 1, 1, 1, 1, 1, 1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ) 

hiperplano <- perceptron(X = X, y = y) 
hiperplano

library(ggplot2) 
datos <- data.frame(X, y) 
ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) +
  geom_point() + 
  # La pendiente e intersección de la recta se obtienen siguiendo los pasos 
  # descritos anteriormente para obtener una recta a partir dos vectores 
  geom_abline(intercept = -(hiperplano[1]/hiperplano[3]),
              slope =     -(hiperplano[2]/hiperplano[3])) + 
  theme_bw() + 
  theme(legend.position = "none") 


#xd