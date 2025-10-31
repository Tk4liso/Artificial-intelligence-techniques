#Práctica del perceptrón múltiple 
library(ggplot2)

# ==== a) ====
curve(sin(x^2+2)*cos(x-1), -4,4)

funct1<-function(x) sin(x^2+2)*cos(x-1)
p<-ggplot(data.frame(x=c(-4,4)), aes(x=x)) + stat_function(fun=funct1)
p

set.seed(42)
x<-runif(100, min=-4, max=4)
y<-funct1(x)

plot(x,y)
curve(sin(x^2+2)*cos(x-1), -4, 4, add=TRUE)

library(nnet)
nn<-nnet(x,y,size=20, maxit=40, linout = TRUE)

grid<-seq(-4,4, by=0.1)
lines(grid, predict(nn, data.frame(x=grid)), col="red")


# ----> Mejorar la aproximación
#plot(x,y)
#curve(sin(x^2+2)*cos(x-1), -4, 4, add=TRUE)

# - Con nnet | Combinatoria del # de neuronas e iteraciones -
library(nnet)
nn<-nnet(x,y,size=40, maxit=80, linout = TRUE)

grid<-seq(-4,4, by=0.1)
lines(grid, predict(nn, data.frame(x=grid)), col="orange")


nn<-nnet(x,y,size=60, maxit=200, linout = TRUE)

grid<-seq(-4,4, by=0.1)
lines(grid, predict(nn, data.frame(x=grid)), col="blue")


# - Con neuralnet -
library(neuralnet)

#Normalización sencilla (min-max) | Neuralnet lo recomienda
normalize <- function(v) (v - min(v)) / (max(v) - min(v))

inv_normalize <- function(v_norm, original) {
  v_norm * (max(original) - min(original)) + min(original)
}

x_norm <- normalize(x)
y_norm <- normalize(y)
data <- data.frame(x = x_norm, y = y_norm)

#Entrenamiento
set.seed(42)
nn_model <- neuralnet(y ~ x, data = data, hidden = c(70,70), linear.output = TRUE) #hidden = # de neuronas en la capa oculta
#Buenos resultados: El mejor - c(20,20,20,20) | Tarda un poco - c(70,70) | c(40,40,40) | c(35,35,35,35)

plot(nn_model) #Plotear la RNA

#Predicción
grid <- seq(-4, 4, by = 0.1)
grid_norm <- normalize(grid)
grid_df <- data.frame(x = grid_norm)

#Desnormalizar
pred_norm <- predict(nn_model, grid_df)
pred <- inv_normalize(pred_norm, y)  #Predicción en escala original


plot(x, y, main = "Aproximación con neuralnet", pch = 16)
curve(funct1, -4, 4, add = TRUE, col = "black", lwd = 2)
lines(grid, pred, col = "forestgreen", lwd = 2)  #Línea verde de la red


# -- Prueba 2 --
# Expandiendo x -> [-4, 6]
grid_ext <- seq(-4, 6, by = 0.1)
grid_ext_norm <- normalize(grid_ext)  #Normalizar con la misma función usada antes
grid_ext_df <- data.frame(x = grid_ext_norm)

#Predecir en el grid extendido
pred_ext_norm <- predict(nn_model, grid_ext_df)
pred_ext <- inv_normalize(pred_ext_norm, y)

plot(x, y, xlim = c(-4, 6), ylim = range(c(y, pred_ext)), pch = 16, main = "Generalización fuera del rango de entrenamiento")
curve(funct1, -4, 6, add = TRUE, col = "black", lwd = 2)
lines(grid_ext, pred_ext, col = "orange", lwd = 2)









# ==== b) ====
library(nnet) 
library(plot3D)    

#Graficar la función de una pirámide
set.seed(42) 
x <- seq(-1, 1, by = 0.01) 
y <- seq(-1, 1, by = 0.01) 
grid <- mesh(x, y) 
z <- with(grid, 1-abs(x+y)-abs(y-x)) 
persp3D(z=z, x=x, y=y, xlab = "X", ylab = "Y", facets=TRUE, theta=10, phi=30) 


#Suponiendo que no conocemos la función y se recopilaron 100 datos de muestras aleatorias
funct<-function(x,y) {1-abs(x+y)-abs(y-x)} 
sample_x <- runif(n=100,-1,1) 
sample_y <- runif(n=100,-1,1) 
sample_z <- funct(sample_x,sample_y) 
scatter3D(sample_x,sample_y,sample_z,theta=10, phi=40) 

nn <- nnet(cbind(sample_x, sample_y), sample_z, size=10, maxit = 40, linout = TRUE)

test_vec = expand.grid(x, y) # gives a 40401x2 test vector 
res=predict(nn, test_vec) 

dim(res)<-c(201, 201) 
persp3D(z = res, x = x, y = y, xlab = "X", ylab = "Y", facets = TRUE, theta = 10, phi = 40)


# ----> Mejorar la aproximación
# - Con nnet -
library(nnet)

nn <- nnet(cbind(sample_x, sample_y), sample_z, size=100, maxit = 5000, linout = TRUE) #20,40 | 30,120 | 20,200 | 20,300 | 30,300 | 50,1000 - 100,1000 | 100,5000

test_vec = expand.grid(x, y) # gives a 40401x2 test vector 
res=predict(nn, test_vec) 

dim(res)<-c(201, 201) 
persp3D(z = res, x = x, y = y, xlab = "X", ylab = "Y", facets = TRUE, theta = 10, phi = 40)







#xd