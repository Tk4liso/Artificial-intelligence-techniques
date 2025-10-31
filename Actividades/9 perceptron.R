perceptron <- function(x, y, eta, niter) {
        
        # inicializar vector de peso
        weight <- rep(0, dim(x)[2] + 1)
        errors <- rep(0, niter)
        
        
        # ciclo sobre el numero de epocas niter
        for (jj in 1:niter) {
                
             # # recorrer el conjunto de datos de entrenamiento
             for (ii in 1:length(y)) {
                      
               # Predecir la etiqueta binaria usando la 
               # funcion de activacion de Heaviside
               z <- sum(weight[2:length(weight)] * 
                            as.numeric(x[ii, ])) + weight[1]
               if(z < 0) {
                    ypred <- -1
               } else {
                    ypred <- 1
               }
                        
               # Cambiar de peso - la formula no hace nada 
               # si el valor predicho es correcto
               weightdiff <- eta * (y[ii] - ypred) * 
                        c(1, as.numeric(x[ii, ]))
               weight <- weight + weightdiff
                       
               # Actualiza funcion de error
               if ((y[ii] - ypred) != 0.0) {
                     errors[jj] <- errors[jj] + 1
               }
                        
        }
        }
        
        # peso para decidir entre las dos especies
        print(weight)
        return(errors)
}

err <- perceptron(x, y, 1, 10)
