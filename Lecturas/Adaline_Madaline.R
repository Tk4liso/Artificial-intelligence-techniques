#Redes Adaline y Madaline

library(tidyverse)

#asumir que x es una matriz con 3 columnas: x1, x2, y

MRI <- function(x, random_seed = 117, alfa = 0.1, max_epochs = 6) {
  set.seed(random_seed)
  
  #1. Inicializar pesos y sesgos
  w <- matrix(runif(4, -0.5, 0.5), nrow = 2)  # w11, w12, w21, w22
  b_hidden <- runif(2, -0.5, 0.5)
  v <- c(0.5, 0.5)
  b_output <- 0.5
  
  #Función de activación bipolar
  f_bipolar <- function(p) ifelse(p >= 0, 1, -1)
  
  for (epoch in 1:max_epochs) {
    cat("Época:", epoch, "\n")
    pesos_modificados <- FALSE
    
    for (i in 1:nrow(x)) {
      x1 <- x[i, 1]
      x2 <- x[i, 2]
      yd <- x[i, 3]
      entrada <- c(x1, x2)
      
      #4. Las activaciones de entrada ya están dadas
      
      #5. Entrada neta de las unidades ocultas
      zin1 <- b_hidden[1] + sum(entrada * w[1, ])
      zin2 <- b_hidden[2] + sum(entrada * w[2, ])
      
      #6. Salida de unidades ocultas
      z1 <- f_bipolar(zin1)
      z2 <- f_bipolar(zin2)
      z <- c(z1, z2)
      
      #7. Calcular la entrada neta de salida
      yin <- b_output + sum(v * z)
      
      #7. (cont.) Salida final
      y <- f_bipolar(yin)
      
      #8. Verificación y ajuste
      if (y != yd) {
        pesos_modificados <- TRUE
        
        if (yd == 1) {
          #Buscar unidad cuya entrada neta esté más cerca de 0
          entradas_net <- c(zin1, zin2)
          idx <- which.min(abs(entradas_net))
          
          #Actualizar sus pesos
          for (j in 1:2) {
            w[idx, j] <- w[idx, j] + alfa * (1 - entradas_net[idx]) * entrada[j]
          }
          b_hidden[idx] <- b_hidden[idx] + alfa * (1 - entradas_net[idx])
        } else if (yd == -1) {
          #Para cada unidad con entrada neta positiva, actualizar
          for (j in 1:2) {
            if (c(zin1, zin2)[j] > 0) {
              for (k in 1:2) {
                w[j, k] <- w[j, k] + alfa * (-1 - c(zin1, zin2)[j]) * entrada[k]
              }
              b_hidden[j] <- b_hidden[j] + alfa * (-1 - c(zin1, zin2)[j])
            }
          }
        }
      }
    }
    
    #9. Condición de paro
    if (!pesos_modificados) {
      cat("Convergencia alcanzada en época", epoch, "\n")
      break
    }
  }
  
  return(list(w = w, b_hidden = b_hidden, v = v, b_output = b_output))
}


MRII <- function(x, random_seed = 117, alfa = 0.1, max_epochs = 6) {
  set.seed(random_seed)
  
  #Inicializar pesos aleatoriamente
  w <- matrix(runif(4, -0.5, 0.5), nrow = 2)  # w11, w12; w21, w22
  b_hidden <- runif(2, -0.5, 0.5)
  v <- c(0.5, 0.5)
  b_output <- 0.5
  
  #Función de activación bipolar
  f_bipolar <- function(p) ifelse(p >= 0, 1, -1)
  
  for (epoch in 1:max_epochs) {
    cat("Época:", epoch, "\n")
    cambios <- FALSE
    
    for (i in 1:nrow(x)) {
      x1 <- x[i, 1]
      x2 <- x[i, 2]
      yd <- x[i, 3]
      entrada <- c(x1, x2)
      
      #5. Entrada neta de unidades ocultas
      zin <- b_hidden + w %*% entrada
      zout <- sapply(zin, f_bipolar)
      
      #7. Entrada neta y salida de la red
      yin <- b_output + sum(zout * v)
      y <- f_bipolar(yin)
      
      error_previo <- (yd - y)^2
      
      #9. para cada unidad cuya entrada neta esté más cerca a 0
      orden <- order(abs(zin))
      for (j in orden) {
        zout_mod <- zout
        zout_mod[j] <- -zout[j]  # cambiar la salida de la unidad j
        
        #10. Recalcular salida
        y_mod <- f_bipolar(b_output + sum(v * zout_mod))
        error_nuevo <- (yd - y_mod)^2
        
        if (error_nuevo < error_previo) {
          #Aplicar regla delta: usar zout_mod como salida deseada para z_j
          zin_j <- zin[j]
          for (k in 1:2) {
            w[j, k] <- w[j, k] + alfa * (zout_mod[j] - zout[j]) * entrada[k]
          }
          b_hidden[j] <- b_hidden[j] + alfa * (zout_mod[j] - zout[j])
          cambios <- TRUE
          break  #solo se ajusta una neurona a la vez
        }
      }
    }
    
    #11. Condición de paro
    if (!cambios) {
      cat("Convergencia alcanzada en época", epoch, "\n")
      break
    }
  }
  
  return(list(w = w, b_hidden = b_hidden, v = v, b_output = b_output))
}



#Tabla de verdad XOR (entradas y salidas bipolares)
xor_data <- matrix(c(
  1,  1, -1,
  1, -1,  1,
  -1,  1,  1,
  -1, -1, -1
), byrow = TRUE, ncol = 3)


m <- MRI(xor_data)
m

m_2<-MRII(xor_data)
m_2


# ==== Ploteo ====
library(igraph)

plot_madaline <- function(model, titulo = "Red Madaline") {
  w <- model$w
  b <- model$b_hidden
  v <- model$v
  b3 <- model$b_output
  
  # Nodos
  nodos <- c("x1", "x2", "1_bias1", "1_bias2", "z1", "z2", "1_bias3", "y")
  aristas <- c("x1", "z1",   # w11
               "x1", "z2",   # w12
               "x2", "z1",   # w21
               "x2", "z2",   # w22
               "1_bias1", "z1",  # b1
               "1_bias2", "z2",  # b2
               "z1", "y",    # v1
               "z2", "y",    # v2
               "1_bias3", "y")   # b3
  
  g <- graph(edges = aristas, directed = TRUE)
  
  # Posición estilo imagen
  layout <- matrix(nrow = 8, ncol = 2)
  layout[which(V(g)$name == "x1"),] <- c(0, 1)
  layout[which(V(g)$name == "x2"),] <- c(0, 0)
  layout[which(V(g)$name == "1_bias1"),] <- c(0, 2)
  layout[which(V(g)$name == "1_bias2"),] <- c(0, -1)
  layout[which(V(g)$name == "z1"),] <- c(1.5, 1)
  layout[which(V(g)$name == "z2"),] <- c(1.5, 0)
  layout[which(V(g)$name == "1_bias3"),] <- c(1.5, 2)
  layout[which(V(g)$name == "y"),] <- c(3, 0.5)
  
  # Valores reales para cada arista
  pesos <- c(w[1,1], w[1,2], w[2,1], w[2,2], b[1], b[2], v[1], v[2], b3)
  
  edge_labels <- c("w11", "w12", "w21", "w22", "b1", "b2", "v1", "v2", "b3")
  edge_labels <- paste0(edge_labels, " = ", round(pesos, 3))
  
  # Colores de los edges según signo
  edge_colors <- ifelse(pesos >= 0, "blue", "red")
  
  # Grosor proporcional al valor absoluto
  edge_widths <- scales::rescale(abs(pesos), to = c(1, 6))
  
  # Colores de nodos
  vertex_colors <- ifelse(grepl("1_bias", V(g)$name), "#faa", "#f77")
  
  plot(g, layout = layout,
       vertex.color = vertex_colors,
       vertex.size = 30,
       vertex.label.cex = 1.2,
       vertex.label.color = "black",
       edge.arrow.size = 0.5,
       edge.curved = 0.2,
       edge.label = edge_labels,
       edge.label.cex = 0.9,
       edge.label.color = edge_colors,
       edge.color = edge_colors,
       edge.width = edge_widths,
       main = titulo)
}


plot_madaline(m, "Red Madaline - MRI")
plot_madaline(m_2, "Red Madaline - MRII")








#xd