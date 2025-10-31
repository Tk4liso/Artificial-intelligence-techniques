#Perceptrón y arquitecturas multicapa

# ==== 1 ====
# Elije las bibliotecas que se usarán
#install.packages("neuralnet")
library(neuralnet)

# Establece el directorio de trabajo para los datos de entrenamiento
#setwd("C:/R")
#getwd()

# Lee el archivo de entrada
mydata=read.csv('C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Técnicas de Inteligencia Artificial\\Actividades\\8 Squares.csv',sep=",",header=TRUE)
mydata
attach(mydata)
names(mydata)

# Entrena el modelo basado en la salida de entrada
model=neuralnet(formula = Output~Input,
                data = mydata,
                hidden=10,
                threshold=0.01 )
print(model)

# Permite trazar y ver las capas
plot(model)

#Comprueba los datos: reales y previstos
final_output=cbind (Input, Output,
                    as.data.frame(model$net.result) )
colnames(final_output) = c("Input", "Expected Output",
                           "Neural Net Output" )
print(final_output)


# ==== 2 ====

# Elije las bibliotecas que se usarán
library(NeuralNetTools)
library(nnet)

# Establece el directorio de trabajo para los datos de entrenamiento 
#setwd("C:/R")
#getwd()

# Lee el archivo de entrada 
mydata=read.csv('C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Técnicas de Inteligencia Artificial\\Actividades\\8 RestaurantTips.csv',sep=",",header=TRUE)
mydata
attach(mydata)
names(mydata)

# Entrena el modelo basado en la salida de entrada
model=nnet(CustomerWillTip~Service+Ambience+Food,
           data = mydata,
           size=5,
           rang=0.1,
           decay=5e-2, 
           maxit=5000) 

print(model)
plotnet(model)
garson(model)














#xd