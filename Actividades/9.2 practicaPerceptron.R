#Ejemplo del Perceptrón

# cargar conjunto de datos de iris 
data(iris) 

# subconjunto del marco de datos del iris: extraer solo especies versicolor y setosa 
# solo nos centraremos en las longitudes de sépalos y pétalos del conjunto de datos 
irissubdf <- iris[1:100, c(1, 3, 5)] 
names(irissubdf) <- c("sepal", "petal", "species") 
head(irissubdf) 

# trazar datos: una imagen vale más que 1000 palabras. melt data => luego ggplot 
library(ggplot2) # no olvides instalar primero este paquete install.packages("ggplot2") 

ggplot(irissubdf, aes(x = sepal, y = petal)) +  
  geom_point(aes(colour=species, shape=species), size = 3) + 
  xlab("sepal length") + 
  ylab("petal length") + 
  ggtitle("Species vs sepal and petal lengths") 


irissubdf[, 4] <- 1 
irissubdf[irissubdf[, 3] == "setosa", 4] <- -1 
x <- irissubdf[, c(1, 2)] 
y <- irissubdf[, 4] 

# cabeza y cola de datos 
head(x)
head(y)


#Cargar el archivo R
source('C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Técnicas de Inteligencia Artificial\\Actividades\\9 perceptron.R') 

plot(1:10, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors") 
title("Errors vs epoch - learning rate eta = 1") 


irisdata <- iris[, c(1, 3, 5)] 
names(irisdata) <- c("sepal", "petal", "species") 
  
ggplot(irisdata, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) + 
  xlab("sepal length") + 
  ylab("petal length") +  
  ggtitle("Species vs sepal and petal lengths") 


# subconjunto de propiedades de las flores del conjunto de datos de iris 
x <- iris[, 1:4] 
names(x) <- tolower(names(x)) 

# crear etiquetas de especies
y <- rep(-1, dim(x)[1]) 
y[iris[, 5] == "virginica"] <- 1 

# calcular y trazar el error 
err <- perceptron(x, y, 0.01, 50)     # seria interesante cambiar los parametros y graficar el error 

# si cambias los parametros, por ejemplo mas epocas, no olvides cambiar el rango del plot 
plot(1:50, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors") 
title("Errors in differentiating Virginica vs epoch - learning rate eta = 0.01") 






#xd