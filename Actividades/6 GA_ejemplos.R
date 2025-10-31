# ==== Ejemplo de un AG ====

# ----> 1. Inicializar la población
population<-c(1,3,0)

# ----> 2. Evaluar la aptitud

a=1
b=-4
c=4
f=function(x){
  a*x^2+b*x+c
}

#Definir el dominio sobre el que se graficará f(x)
library(dplyr)
library(ggplot2)

x<-seq(from=0, to=4, length.out=100)

#Crear DF
df<-x|>
  data.frame(x=_)|>
  dplyr::mutate(y=f(x))

#Definir el espacio donde queremos probar
possible_xvalues<-seq(from=0, to=3, lenght.out=4)

#Crear un DF especial
space<-possible_xvalues|>
  data.frame(x=_)|>
  dplyr::mutate(y=f(x))

#Calcular la aptitud en línea
fitness<-population^2-4 * population+4

#Seleccionar a los padres supervivientes
num_parents<-2
selected_parents<-population|>
  order(fitness,decreasing=FALSE)|>
  head(num_parents)

#Graficar f(x) con ggplot 
ggplot2::ggplot(df, aes(x = x, y = y)) + 
  geom_line(color = "black") +  # Plot the function as a line 
  #Plot points at x=1 and x=3 
  geom_point(data = subset(space, x %in% c(1,3)), color = "coral1", size = 3, shape = 8) +   
  #Plot a point at x=0 
  geom_point(data = subset(space, (x ==0 )), color = "blue", size = 3, shape = 8) +   
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +  # Add horizontal line at y=0 
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +  # Add vertical line at x=0 
  theme_minimal() 


# ----> 3. Selección

# Plot f(x) using ggplot 
ggplot(df, aes(x = x, y = y)) + 
  geom_line(color = "black") +  # Plot the function as a line 
  geom_point(data = subset(space, x %in% c(1, 3)), color = "coral1", size = 6, shape = 8) + 
  #Plot points at x=1 and x=3  
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  # Add horizontal line at y=0 
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") + 
  # Add vertical line at x=0 
  theme_minimal()  # Use a minimal theme 


# ----> 4.Cruce y mutación | 5.Reemplazo | 6.Repetir los pasos 2 a 5 para varias generaciones
find.fitting = function(a, b, c) { 
  x_fitting = -b/(2 * a) 
  y_fitting = f(x_fitting) 
  c(x_fitting, y_fitting) 
}

F = find.fitting(a, b, c)

# Plot f(x) using ggplot 
ggplot(df, aes(x = x, y = y)) + 
  geom_line(color = "black") +  # Plot the function as a line 
  geom_hline(yintercept = 0, linetype = "dashed") +  # Add horizontal line at y=0 
  geom_vline(xintercept = 0, linetype = "dashed") +  # Add vertical line at x=0 
  geom_point(x = F[1], y = F[2], shape = 18, size = 6, color = "red") +  # Plot the vertex 
  geom_text(x = F[1], y = F[2], label = "Fitting", vjust = -1, color = "red", size = 5) + 
  # Add label next to the vertex 
  theme_minimal()  # Use a minimal theme


#Solución alternativa
# find the x-intercepts of f(x) 
find.roots = function(a, b, c) { 
  discriminant = b^2 - 4 * a * c 
  if (discriminant > 0) { 
    c((-b - sqrt(discriminant))/(2 * a), (-b + sqrt(discriminant))/(2 * a)) 
    } 
  else if (discriminant == 0) { 
    -b / (2 * a)
    } 
  else { 
    Nan 
    } 
} 

solutions = find.roots(a, b, c) 
solutions 

# Plot f(x) using ggplot 
ggplot(df, aes(x = x, y = y)) + 
  geom_line(color = "black") +  # Plot the function as a line 
  geom_hline(yintercept = 0, linetype = "dashed") +  # Add horizontal line at y=0 
  geom_vline(xintercept = 0, linetype = "dashed") +  # Add vertical line at x=0 
  geom_point(data = data.frame(x = solutions, y = rep(0, length(solutions))), shape = 18,
                 size = 6, color = "red") +  # Plot x-intercepts 
  geom_text(data = data.frame(x = solutions, y = rep(0, length(solutions)),
                                    label = "Fitting(x-intercept)"), aes(label = label), vjust = -1, color = "red", size = 5) +  
  # Add labels next to x-intercepts 
  theme_minimal()  # Use a minimal theme 



# ==== Ejemplo de AG, regresión simbólica  ====
#generamos algunos datos sobre la base de una combinación de funciones trigonométricas
#y = sin(x) + cos (x + x) 

x<-seq(0,4*pi, length.out=201)
y<-sin(x)+cos(x+x)
plot(y)

library(gramEvol)
ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos), 
                op = grule('+', '-', '*'), 
                var = grule(x)) 
grammarDef <- CreateGrammar(ruleDef) 
grammarDef 

#ejemplos de fórmulas creadas aleatoriamente a partir de esta gramática
set.seed(123)
GrammarRandomExpression(grammarDef,6)

#definir alguna función de costo para evaluar qué tan buena es la fórmula respectiva
SymRegFitFunc <- function(expr) { 
  result <- eval(expr) 
  if (any(is.nan(result))) 
    return(Inf) 
  return (mean(log(1 + abs(y - result)))) 
} 

set.seed(314) 
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 2500, max.depth = 5)
ge

plot(y)
points(eval(ge$best$expressions), col = "red", type = "l") 


#ejecutamos el ejemplo nuevamente, pero esta vez con algo de ruido agregado
x <- seq(0, 4*pi, length.out = 201) 
y <- jitter(sin(x) + cos(x + x), amount = 0.2) 
plot(y) 

ruleDef <- list(expr = grule(op(expr, expr), func(expr), var), 
                func = grule(sin, cos), 
                op = grule('+', '-', '*'), 
                var = grule(x)) 

grammarDef <- CreateGrammar(ruleDef) 
grammarDef 


SymRegFitFunc <- function(expr) {
  result <- eval(expr) 
  if (any(is.nan(result))) 
    return(Inf) 
  return (mean(log(1 + abs(y - result)))) 
} 

set.seed(314) 
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 2500, max.depth = 5) 
ge 

plot(y)
points(eval(ge$best$expressions), col = "red", type = "l") 



#encontrar la relación entre los períodos orbitales y las distancias al sol de nuestro sistema solar
planets <- c("Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus") 
distance <- c(0.72, 1.00, 1.52, 5.20, 9.53, 19.10) 
period <- c(0.61, 1.00, 1.84, 11.90, 29.40, 83.50) 
data.frame(planets, distance, period) 

ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, tan, log, sqrt), 
                op = grule('+', '-', '*', '/', '^'), 
                var = grule(distance, n), 
                n = grule(1, 2, 3, 4, 5, 6, 7, 8, 9)) 
grammarDef <- CreateGrammar(ruleDef) 
grammarDef 

SymRegFitFunc <- function(expr) { 
  result <- eval(expr) 
  if (any(is.nan(result))) 
    return(Inf) 
  return (mean(log(1 + abs(period - result)))) 
}

set.seed(2)
suppressWarnings(ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.05)) 
ge #el algoritmo acaba de redescubrir la tercera ley de Kepler en muy poco tiempo



# ==== Algoritmos genéticos: Un ejemplo simple en R ====
library(genalg)
library(ggplot2)

dataset <- data.frame(articulo = c("navaja", "frijoles", "papas", "uniones","saco dormir", "cuerda", "brujula"), puntosSupervivencia = c(10, 20, 15, 2, 30, 10, 30), peso = c(1, 5, 10, 1, 7, 5, 1)) 
pesoLimite <- 20 

cromosoma=c(1,0,0,1,1,0,0)
dataset[cromosoma==1,]
cat(cromosoma %*% dataset$puntosSupervivencia)

#Definir la función de evaluación
evalFunc<-function(x){
  solucion_actual_puntosSupervivencia<-x %*% dataset$puntosSupervivencia
  solucion_actual_peso<-x %*% dataset$peso
  if(solucion_actual_peso > pesoLimite)
    return(0)
  else
    return(-solucion_actual_puntosSupervivencia)
    #return(solucion_actual_puntosSupervivencia)
}

#Diseñral el modelo
iter=100
GAmodel<-rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))

solucion=c(1,1,0,1,1,1,1)
dataset[solucion==1,]

#solución vs disponible
cat(paste(solucion %*% dataset$puntosSupervivencia, "/", sum(dataset$puntosSupervivencia)))

#Ver como evoluciona el modelo
library(ggplot2)
library(gganimate)
library(dplyr)

animate_plot <- function(iter, GAmodel) { 
  for (i in seq(1, iter)) { 
    temp <- data.frame(
      Generacion = c(seq(1, i), seq(1, i)),
      Variable = c(rep("media", i), rep("mejor", i)),
      PuntosSupervivencia = c(-GAmodel$mean[1:i], -GAmodel$best[1:i])  # Ajustar valores
    )
    
    pl <- ggplot(temp, aes(x = Generacion, y = PuntosSupervivencia, group = Variable, colour = Variable)) +
      geom_line() +
      scale_x_continuous(limits = c(0, iter)) +
      scale_y_continuous(limits = c(min(temp$PuntosSupervivencia, na.rm = TRUE), max(temp$PuntosSupervivencia, na.rm = TRUE))) +
      geom_hline(yintercept = max(temp$PuntosSupervivencia, na.rm = TRUE), lty = 2) +
      annotate("text", x = 1, y = max(temp$PuntosSupervivencia, na.rm = TRUE) + 2, hjust = 0, size = 3, color = "black", 
               label = paste("Mejor solucion:", max(temp$PuntosSupervivencia, na.rm = TRUE))) +
      scale_colour_brewer(palette = "Set1") +
      ggtitle("Evolución del modelo de optimización de la Mochila")
    
    print(pl)
  } 
}

ani.options(interval = 0.1)  # Ajusta el intervalo de la animación
anim_save("evolucion_mochila.gif", animate_plot(iter, GAmodel))



#Alternativa 2 de la animación (mismo resultado)
library(ggplot2)
library(gganimate)
library(dplyr)

# Crear el dataframe con la evolución de la media y el mejor resultado por generación
temp <- data.frame(
  Generacion = rep(1:iter, 2),
  Variable = rep(c("media", "mejor"), each = iter),
  PuntosSupervivencia = c(-GAmodel$mean[1:iter], -GAmodel$best[1:iter]) # Ajustamos el signo
)

# Crear la animación con gganimate
pl <- ggplot(temp, aes(x = Generacion, y = PuntosSupervivencia, group = Variable, colour = Variable)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, iter)) +
  scale_y_continuous(limits = c(min(temp$PuntosSupervivencia, na.rm = TRUE), max(temp$PuntosSupervivencia, na.rm = TRUE))) +
  geom_hline(yintercept = max(temp$PuntosSupervivencia, na.rm = TRUE), lty = 2) +
  annotate("text", x = 1, y = max(temp$PuntosSupervivencia, na.rm = TRUE) + 2, hjust = 0, size = 3, color = "black", 
           label = paste("Mejor solución:", max(temp$PuntosSupervivencia, na.rm = TRUE))) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Evolución del modelo de optimización de la Mochila") +
  transition_reveal(Generacion)  # Agregamos transición para animación

# Guardar la animación como GIF
anim_save("evolucion_mochila-2.gif", animate(pl, duration = 10, fps = 10, width = 800, height = 600))




# ----> Lo mismo pero con más artículos de supervivencia
library(genalg)
library(ggplot2)

dataset <- data.frame(articulo = c("Navaja", "Frijoles", "Papas", "Uniones", "Saco de dormir", "Cuerda", "Brújula","Botella de agua", "Linterna", "Encendedor", "Kit de primeros auxilios", "Mapa", "Guantes térmicos"),
  puntosSupervivencia = c(10, 20, 15, 2, 30, 10, 30, 25, 15, 10, 40, 5, 20),
  peso = c(1, 5, 10, 1, 7, 5, 1, 3, 2, 1, 5, 1, 2))

pesoLimite <- 20 

#Crear un cromosoma de prueba
cromosoma <- c(1,0,0,1,1,0,0, 1,0,0,1,1,0)
dataset[cromosoma == 1,]  #Ver los artículos seleccionados
cat("Puntos de supervivencia:", cromosoma %*% dataset$puntosSupervivencia, "\n") #Calcular los puntos de supervivencia para la selección

#Definir la función de evaluación
evalFunc<-function(x){
  solucion_actual_puntosSupervivencia<-x %*% dataset$puntosSupervivencia
  solucion_actual_peso<-x %*% dataset$peso
  if(solucion_actual_peso > pesoLimite)
    return(0)
  else
    return(-solucion_actual_puntosSupervivencia)
  #return(solucion_actual_puntosSupervivencia)
}

#Diseñarl el modelo
iter=100
GAmodel<-rbga.bin(size = 13, popSize = 200, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))

solucion=c(1,0,0,0,1,0,1,1,0,1,1,0,1)
dataset[solucion==1,]

#solución vs disponible
cat(paste(solucion %*% dataset$puntosSupervivencia, "/", sum(dataset$puntosSupervivencia)))



#Alternativa 2 de la animación (mismo resultado)
library(ggplot2)
library(gganimate)
library(dplyr)

#Crear el DF con la evolución de la media y el mejor resultado por generación
temp <- data.frame(
  Generacion = rep(1:iter, 2),
  Variable = rep(c("media", "mejor"), each = iter),
  PuntosSupervivencia = c(-GAmodel$mean[1:iter], -GAmodel$best[1:iter]) # Ajustamos el signo
)

pl <- ggplot(temp, aes(x = Generacion, y = PuntosSupervivencia, group = Variable, colour = Variable)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, iter)) +
  scale_y_continuous(limits = c(min(temp$PuntosSupervivencia, na.rm = TRUE), max(temp$PuntosSupervivencia, na.rm = TRUE))) +
  geom_hline(yintercept = max(temp$PuntosSupervivencia, na.rm = TRUE), lty = 2) +
  annotate("text", x = 1, y = max(temp$PuntosSupervivencia, na.rm = TRUE) + 2, hjust = 0, size = 3, color = "black", 
           label = paste("Mejor solución:", max(temp$PuntosSupervivencia, na.rm = TRUE))) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Evolución del modelo de optimización de la Mochila con 13 artículos") +
  transition_reveal(Generacion)  # Agregamos transición para animación

anim_save("evolucion_mochila-2.gif", animate(pl, duration = 10, fps = 10, width = 800, height = 600))





#xd