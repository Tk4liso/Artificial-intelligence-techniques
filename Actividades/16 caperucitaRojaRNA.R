#El problema de la caperucita roja 

library(neuralnet) 
library(NeuralNetTools) 

# 1. Definir características (qualities)
qualities <- matrix (c(1, 1, 1, 0, 0, 0,  #lobo
                       0, 1, 0, 1, 1, 0,  #abuela
                       1, 0, 0, 1, 0, 1   #leñador
                       ), byrow = TRUE, nrow = 3) 
colnames(qualities) <- c("orejas_grandes", "ojos_grandes", "dientes_grandes", "dulce_amable", "arrugado", "guapo") 
rownames(qualities) <- c("lobo", "abuelita", "legnador") 
qualities 

# 2. Definir acciones (actions)
actions <- matrix (c(1, 1, 1, 0, 0, 0, 0, #lobo
                     0, 0, 0, 1, 1, 1, 0, #abuela
                     0, 0, 0, 1, 0, 1, 1  #leñador
                     ), byrow = TRUE, nrow = 3) 
colnames(actions) <- c("huir", "gritar", "busca_legnador", "besar_en_la_mejilla", "acercarse", "ofrecer_comida", "coquetear_con") 
rownames(actions) <- rownames(qualities) 
actions 

# 3. Combinar datos
data <- cbind(qualities, actions) 

# 4. Entrenar la red neuronal (RNA)
set.seed(123)
neuralnetwork <- neuralnet(huir + gritar + busca_legnador + besar_en_la_mejilla + acercarse + 
                           ofrecer_comida + coquetear_con ~  
                           orejas_grandes + ojos_grandes + dientes_grandes + dulce_amable + arrugado + guapo, 
                           data = data, 
                           hidden = 5, 
                           #exclude = c(1, 8, 15, 22, 26, 30, 34, 38, 42, 46), 
                           lifesign = "minimal", linear.output = FALSE) 

# 5. Visualizar la red
par_bkp <- par(mar = c(0, 0, 0, 0)) # set different margin to minimize cutoff text 
plotnet(neuralnetwork, bias = FALSE)



# ==== Pruebas ====

test_inputs <- data.frame(
  orejas_grandes = c(1, 0, 1, 0, 1),
  ojos_grandes =   c(1, 0, 1, 1, 1),
  dientes_grandes = c(0,1, 1, 1, 0),
  dulce_amable =   c(0, 1, 1, 1, 1),
  arrugado =       c(0, 1, 0, 1, 0),
  guapo =          c(0, 0, 1, 1, 1)
)

rownames(test_inputs) <- c(
  "Caso 1: Lobo sin dientes grandes",
  "Caso 2: Abuelita con dientes grandes",
  "Caso 3: Leñador lobezno",
  "Caso 4: Lobo arrugado, guapo y amable",
  "Caso 5: Leñador furry"
)

#Obtener predicciones
pred <- compute(neuralnetwork, test_inputs)$net.result
pred_rounded <- round(pred, 0)

#Mostrar resultados
resultados <- cbind(test_inputs, round(pred, 2), pred_rounded)
print(resultados)

#Columna	Acción
#1	      huir
#2	      gritar
#3	      busca_legnador
#4	      besar_en_la_mejilla
#5	      acercarse
#6	      ofrecer_comida
#7	      coquetear_con


#Ajustar márgenes para que se vea bien el gráfico
par(mar = c(0, 0, 0, 0))
#Plotear con sesgos visibles
plotnet(neuralnetwork, bias = TRUE)  

neuralnetwork$result.matrix
neuralnetwork$weights



#xd