#Algoritmos genéticos - Tercer examen parcial
#Taisen Romero Bañuelos (202055209) 25/03/2025

# ==== ASCII ====

#˚　　　　✦　　　.　　. 　.˚　.　　　　　 . ✦　　　 　˚　　　　 . ★⋆. ࿐࿔ 
#... 　　˚　　 　　*　　 　　✦　　　.　　.　　　✦　˚ 　　　　.˚　.˚　　　　✦　　　.　　. 　.˚　.
#⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣦⠀
#⠀⠀⠀⠀⣰⣿⡟⢻⣿⡟⢻⣧
#⠀⠀⠀⣰⣿⣿⣇⣸⣿⣇⣸⣿
#⠀⠀⣴⣿⣿⣿⣿⠟⢻⣿⣿⣿
#⣠⣾⣿⣿⣿⣿⣿⣤⣼⣿⣿⠇
#⢿⡿⢿⣿⣿⣿⣿⣿⣿⣿⡿⠀
#⠀⠀⠈⠿⠿⠋⠙⢿⣿⡿⠁⠀


# ==== Implementación del ejemplo del PDF ====

#Población inicial
poblacion <- c("011", "001", "110", "010")
beneficio <- c(3.0, 1.0, 6.0, 2.0)

generacion_0 <- data.frame(
  Bar = 1:4,
  Bebida = poblacion,
  Beneficio = beneficio)

print(generacion_0)

#Métricas de la generación
total_beneficio <- sum(beneficio)
peor <- min(beneficio)
mejor <- max(beneficio)
media <- mean(beneficio)

cat("Total:", total_beneficio, "\n")
cat("Peor:", peor, "\n")
cat("Mejor:", mejor, "\n")
cat("Media:", media, "\n")


proporcion <- beneficio / sum(beneficio) #Calcular proporción de adaptación
generacion_0$Proporcion <- round(proporcion, 2)


#Selección proporcional (tipo ruleta)
set.seed(123)

seleccionados_idx <- sample(1:4, size = 4, replace = TRUE, prob = proporcion)

#Obtener individuos seleccionados
seleccionados <- poblacion[seleccionados_idx]
beneficio_sel <- beneficio[seleccionados_idx]

fase_seleccion <- data.frame(
  Seleccionado = seleccionados,
  Beneficio = beneficio_sel)

total_sel <- sum(beneficio_sel)
peor_sel <- min(beneficio_sel)
mejor_sel <- max(beneficio_sel)
media_sel <- mean(beneficio_sel)

generacion_0
#seleccionados #en mi caso fueron diferentes a los del PDF (sólo 1)
fase_seleccion
total_sel
peor_sel
mejor_sel
media_sel


#Definimos las parejas (índices de los seleccionados)
parejas <- list(c(1, 2), c(3, 4)) #para que no se reproduzcan consigomismo
punto_corte <- 2  # según la tabla
#punto_corte <- sample(1:2, 1)  # si queremos aleatorio entre las posiciones válidas

#Función de cruce
cruce <- function(p1, p2, punto) {
  descendiente1 <- paste0(substr(p1, 1, punto), substr(p2, punto + 1, 3))
  descendiente2 <- paste0(substr(p2, 1, punto), substr(p1, punto + 1, 3))
  return(c(descendiente1, descendiente2))
}

#Aplicar el cruce a las parejas
descendientes <- c()
for (par in parejas) {
  p1 <- seleccionados[par[1]]
  p2 <- seleccionados[par[2]]
  nuevos <- cruce(p1, p2, punto_corte)
  descendientes <- c(descendientes, nuevos)
}

descendientes



#Seleccionados
Ii_prima <- c("110", "010", "110", "010")
f_Ii_prima <- c(6.0, 2.0, 6.0, 2.0)

#Descendientes
Ii_dobleprima <- c("110", "010", "110", "010")
f_Ii_dobleprima <- c(6.0, 2.0, 6.0, 2.0)

tabla_resumen <- data.frame(
  Bar = 1:4,
  Bebida = generacion_0$Bebida,
  Beneficio = generacion_0$Beneficio,
  Bebida_prim1 = Ii_prima,
  Beneficio_prim1 = f_Ii_prima,
  Bebida_prim2 = Ii_dobleprima,
  Beneficio_prim2 = f_Ii_dobleprima)

tabla_resumen

cat("Total:", 
    sum(tabla_resumen$Beneficio), " ", 
    sum(tabla_resumen$Beneficio_prim1), " ", 
    sum(tabla_resumen$Beneficio_prim2), "\n")

cat("Peor :", min(tabla_resumen$Beneficio), " ", min(tabla_resumen$Beneficio_prim1), " ", min(tabla_resumen$Beneficio_prim2))

cat("Mejor:", max(tabla_resumen$Beneficio), " ", max(tabla_resumen$Beneficio_prim1), " ", max(tabla_resumen$Beneficio_prim2))

cat("Media:", mean(tabla_resumen$Beneficio), " ", mean(tabla_resumen$Beneficio_prim1), " ", mean(tabla_resumen$Beneficio_prim2))



# ==== Mi solución/propuesta ====

#Mi solución se basa en la inclusión de una fórmula para el cálculo de f(Li) 
#para que no favorezca a las bebidas que tienen más 1's, pues ese favoritismo
#significaría en el problema que se favorecen aquellas bebidas que tienen todos
#los ingredientes clasificados con "1". Además de que significaría que favorece
#las bebidas que son un licuado de todo, estaríamos excluyendo injustamente
#a aquellos ingredientes clasificados con "0".
#Además, los valores de beneficio parece que están dados empíricamente y no 
#bajo una fórmula con la que podemos proyectar las generaciones que incluyan
#aquellas combinaciones de las que no sabemos su valor empírico.

#Mi fórmula se basa en el supuesto de que la interacción entre los ingredientes
#no favorece las bebidas con más 1's. Todas las combinaciones puntúan 6, es 
#decir, todas tienen el mismo valor base. Pero luego se introduce una 
#penalización o bonificación por combinaciones no deseadas.Así evitamos que el
#algoritmo beneficie las combinaciones que tienen un 1 sólo porque si.


# ==== Definición de función f(Li) con interacciones entre ingredientes ====

#Función que calcula el beneficio (vease la similitud con reglas de asociación)
f_Li <- function(bits_string) {
  #bits_string: cadena binaria como "110"
  bits <- as.integer(strsplit(bits_string, "")[[1]])
  b1 <- bits[1]  #limón
  b2 <- bits[2]  #ron
  b3 <- bits[3]  #coca
  
  score <- 6  #base neutral
  
  if (b1 == 1 && b3 == 0) score <- score - 1  #Penalización: limón natural + coca light
  if (b1 == 0 && b2 == 1) score <- score + 1  #Bonificación: limón con agua + ron añejo
  if (b2 == 0 && b3 == 0) score <- score + 1  #Bonificación: ron blanco + coca light
  if (b2 == 1 && b3 == 1) score <- score - 1  #Penalización: ron añejo + coca normal
  
  return(score)
}

# ----> Generación 0

# Población inicial
poblacion <- c("011", "001", "110", "010")
beneficio <- sapply(poblacion, f_Li)  #Evaluación automática

generacion_0 <- data.frame(
  Bar = 1:4,
  Bebida = poblacion,
  Beneficio = beneficio)

rownames(generacion_0) <- NULL
generacion_0

#Métricas de la generación
total_beneficio <- sum(beneficio)
peor <- min(beneficio)
mejor <- max(beneficio)
media <- mean(beneficio)

total_beneficio
peor
mejor
media


#Proporción de adaptación
proporcion <- beneficio / total_beneficio
generacion_0$Proporcion <- round(proporcion, 2)
generacion_0

# ----> Selección proporcional (ruleta)
set.seed(123)
seleccionados_idx <- sample(1:4, size = 4, replace = TRUE, prob = proporcion)

#Obtener individuos seleccionados
seleccionados <- poblacion[seleccionados_idx]
beneficio_sel <- sapply(seleccionados, f_Li)

fase_seleccion <- data.frame(
  Seleccionado = seleccionados,
  Beneficio = beneficio_sel)

#Métricas post-selección
total_sel <- sum(beneficio_sel)
peor_sel <- min(beneficio_sel)
mejor_sel <- max(beneficio_sel)
media_sel <- mean(beneficio_sel)

# ----> Cruce
parejas <- list(c(1, 2), c(3, 4))  #Sin cruce consigo mismo
punto_corte <- 2  #fijo para replicar el ejemplo

cruce <- function(p1, p2, punto) {
  descendiente1 <- paste0(substr(p1, 1, punto), substr(p2, punto + 1, 3))
  descendiente2 <- paste0(substr(p2, 1, punto), substr(p1, punto + 1, 3))
  return(c(descendiente1, descendiente2))
}

descendientes <- c()
for (par in parejas) {
  p1 <- seleccionados[par[1]]
  p2 <- seleccionados[par[2]]
  nuevos <- cruce(p1, p2, punto_corte)
  descendientes <- c(descendientes, nuevos)
}

beneficio_descendientes <- sapply(descendientes, f_Li)

# ----> Tabla resumen
tabla_resumen <- data.frame(
  Bar = 1:4,
  Bebida = generacion_0$Bebida,
  Beneficio = generacion_0$Beneficio,
  Bebida_prim1 = seleccionados,
  Beneficio_prim1 = beneficio_sel,
  Bebida_prim2 = descendientes,
  Beneficio_prim2 = beneficio_descendientes)

rownames(tabla_resumen) <- NULL
tabla_resumen

cat("Total:", sum(tabla_resumen$Beneficio), " ", sum(tabla_resumen$Beneficio_prim1), " ", sum(tabla_resumen$Beneficio_prim2))

cat("Peor :", min(tabla_resumen$Beneficio), " ", min(tabla_resumen$Beneficio_prim1), " ", min(tabla_resumen$Beneficio_prim2))

cat("Mejor:", max(tabla_resumen$Beneficio), " ", max(tabla_resumen$Beneficio_prim1), " ", max(tabla_resumen$Beneficio_prim2))

cat("Media:", mean(tabla_resumen$Beneficio), " ", mean(tabla_resumen$Beneficio_prim1), " ", mean(tabla_resumen$Beneficio_prim2))


# ==== Avanzar una generación más (mutación incluida) ====

#Función de mutación
mutar <- function(cromosoma, prob = 0.1) {
  bits <- strsplit(cromosoma, "")[[1]]
  for (i in 1:length(bits)) {
    if (runif(1) < prob) {
      bits[i] <- ifelse(bits[i] == "0", "1", "0")
    }
  }
  return(paste0(bits, collapse = ""))
}


#Partiendo desde donde nos quedamos, tenemos que aplicar la mutación a los descendientes
set.seed(456)
descendientes_mutados <- sapply(descendientes, mutar, prob = 0.1)

#Evaluar beneficio de la nueva gen.
beneficio_gen2 <- sapply(descendientes_mutados, f_Li)

generacion_2 <- data.frame(
  Bar = 1:4,
  Bebida = descendientes_mutados,
  Beneficio = beneficio_gen2
)
rownames(generacion_2) <- NULL
generacion_2

#Métricas descendientes generación 2
cat("Total:", sum(beneficio_gen2))
cat("Peor :", min(beneficio_gen2))
cat("Mejor:", max(beneficio_gen2))
cat("Media:", mean(beneficio_gen2))

#Agregar Gen 2 (descendientes mutados) a la tabla de resumen
tabla_resumen$Bebida_prim3 <- descendientes_mutados
tabla_resumen$Beneficio_prim3 <- beneficio_gen2

rownames(tabla_resumen) <- NULL
tabla_resumen


# ==== Comparación final ====

# ----> Enfoque 1: función lineal descubierta con Gauss (como lo había hecho manual al inicio ahorita lo hago como función)
f_lineal <- function(bits_string) {
  bits <- as.integer(strsplit(bits_string, "")[[1]])
  b1 <- bits[1]
  b2 <- bits[2]
  b3 <- bits[3]
  return(4 * b1 + 2 * b2 + 1 * b3)
}

#Convertir decimal a binario de 3 bits
decimal_a_binario <- function(n) {
  bin <- rev(as.integer(intToBits(n)))
  paste0(tail(bin, 3), collapse = "")
}

#Generar las 8 combinaciones posibles de 3 bits (000 a 111)
todas_las_combinaciones <- sapply(0:7, decimal_a_binario)


beneficio_enfoque1 <- sapply(todas_las_combinaciones, f_lineal)
beneficio_enfoque2 <- sapply(todas_las_combinaciones, f_Li)  # tu función

comparativa <- data.frame(
  Combinacion = todas_las_combinaciones,
  Beneficio_Enfoque1 = beneficio_enfoque1,
  Beneficio_Enfoque2 = beneficio_enfoque2)

comparativa







#xd