#05/05/2025
#TIA - Proyecto final
#Solving the 0-1 Knapsack Problem with Genetic Algorithms
#Alumno: Taisen Romero Bañuelos (202055209).

# ==== Primer acercamiento ====
#Implementación reutilizando elementos de la práctica "Algoritmo genético para resolver el problema de la mochila"

library(GA)
library(dplyr)
library(ggplot2)
library(tictoc)

df_item_long <- data.frame(
  item = c("A", "B", "C"),
  weight = c(6, 7, 8),
  benefit = c(4, 3, 5)
)

weightlimit <- 13 #Capacidad máxima en la mochila

#FUNCIÓN DE APTITUD (fitness)
evalFunc <- function(x){
  df <- df_item_long[x == 1, ]
  total_weight <- sum(df$weight)
  total_benefit <- sum(df$benefit)
  
  if (total_weight > weightlimit) {
    #Penalización proporcional al exceso
    penalty <- 10 * (total_weight - weightlimit)
    return(total_benefit - penalty)
  }
  
  return(total_benefit)
}


set.seed(123) 
tic()
gann_mod <- ga(
  type = "binary",
  fitness = evalFunc,
  popSize = 200,        
  maxiter = 300,        
  run = 50,             #Parar si no mejora tras 50 generaciones
  nBits = nrow(df_item_long),
  pmutation = 0.001,    #Prob. de mutación
  pcrossover = 0.85,    #Prob. de cruce
  elitism = 2           #Como en el artículo
)
toc()

summary(gann_mod)
#plot(gann_mod)

#Plot arreglado | Mover leyendas a la esquina sup. izq.
plot(gann_mod, legend = FALSE)
legend("topleft", 
       legend = c("Best", "Mean", "Median"), 
       col = c("green", "blue", "lightgreen"), 
       lty = c(1, 1, 1), 
       lwd = 2, 
       bty = "n")  #bty = "n" elimina el borde de la caja



#EXTRACCIÓN DE LA MEJOR SOLUCIÓN
df_sol <- df_item_long[gann_mod@solution[1,] == 1, ]
df_sol <- df_sol %>%
  group_by(item, weight, benefit) %>%
  summarise(freq = n()) %>%
  mutate(
    total_weight = freq * weight,
    total_benefit = freq * benefit
  )

print(df_sol)
cat("Peso total:", sum(df_sol$total_weight), "\n")
cat("Beneficio total:", sum(df_sol$total_benefit), "\n")

#GRÁFICO DE CONVERGENCIA
df_convergencia <- data.frame(
  Generacion = 1:length(gann_mod@fitness),
  BeneficioMaximo = gann_mod@fitness
)

ggplot(df_convergencia, aes(x = Generacion, y = BeneficioMaximo)) +
  geom_line(size = 0.5) +
  geom_point(color = "red") +
  labs(
    title = "Convergencia del algoritmo genético",
    x = "Generación",
    y = "Beneficio máximo"
  ) +
  theme_minimal(base_size = 14)


ggplot(df_item_long, aes(x = weight, y = benefit, label = item)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_text(vjust = -1) +
  labs(title = "Relación entre peso y beneficio",
       x = "Peso", y = "Beneficio") +
  theme_minimal()




# ==== Continuación de la investigación ====

#Ampliar el número de ítems para observar mejor si el AG sigue convergiendo | 3 ítems es un poco trivial
set.seed(42)
df_item_long <- data.frame(
  item = LETTERS[1:10], #De la 'A' hasta la 'J'
  weight = round(rlnorm(10, meanlog = 2, sdlog = 0.5)), #Mayoría de pesos bajos, algunos muy altos
  benefit = round(runif(10, min = 10, max = 100))
)

weightlimit <- 20 #Le sumé el # de ítems que agregué (7)
#ALTERNATIVA:
#weightlimit <- round(sum(df_item_long$weight) * 0.4)  # 40% del total de pesos

print(df_item_long)
#En caso de que se use el enfoque del 40%:
#cat("Capacidad de la mochila:", weightlimit, "\n")



#Función de monitoreo (registra el porcentaje de soluciones válidas por generación)
registro_validos <- list()
monitor_func <- function(obj){
  pops <- obj@population
  gen <- obj@iter
  valid <- apply(pops, 1, function(x) sum(df_item_long$weight[x == 1]) <= weightlimit)
  registro_validos[[gen]] <<- mean(valid)
}


set.seed(42)
tic()
gann_mod_ext <- ga(
  type = "binary",
  fitness = evalFunc,
  popSize = 200,
  maxiter = 100,
  run = 50,
  nBits = nrow(df_item_long),
  pmutation = 0.001,
  pcrossover = 0.85,
  elitism = 2,
  monitor = monitor_func
)
toc()

summary(gann_mod_ext)
plot(gann_mod_ext)

#Mostrar ítems seleccionados
df_sol <- df_item_long[gann_mod_ext@solution[1,] == 1, ]
df_sol <- df_sol %>%
  group_by(item, weight, benefit) %>%
  summarise(freq = n(), .groups = "drop") %>%
  mutate(
    total_weight = freq * weight,
    total_benefit = freq * benefit
  )

print(df_sol)
df_sol$total_weight
df_sol$total_benefit


#Ploteos
if (length(registro_validos) > 0) {
  df_val <- data.frame(
    Generacion = 1:length(registro_validos),
    PorcentajeValidos = unlist(registro_validos)
  )
  ggplot(df_val, aes(x = Generacion, y = PorcentajeValidos)) +
    geom_line(color = "blue") +
    labs(title = "Porcentaje de soluciones válidas por generación",
         y = "Proporción válida", x = "Generación") +
    theme_minimal()
}


ggplot(df_item_long, aes(x = weight, y = benefit, label = item)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_text(vjust = -1) +
  labs(title = "Relación entre peso y beneficio",
       x = "Peso", y = "Beneficio") +
  theme_minimal()



# ==== Implementación manual (sin paquete GA) ====
#El chiste de hacerlo manual es hacerlo más fiel al artículo. En las versiones 
#anteriores no usé group selection ni ruleta. Intenté hacerlo con el paquete GA
#pero era difícil de implementar, así que mejor lo hice manual

library(dplyr)
library(ggplot2)

set.seed(42)
df_item_long <- data.frame(
  item = LETTERS[1:10],
  weight = round(rlnorm(10, meanlog = 2, sdlog = 0.5)),
  benefit = round(runif(10, min = 10, max = 100))
)

weightlimit <- 20
n_items <- nrow(df_item_long)

#Parámetros del GA
pop_size <- 200
max_gen <- 100
pcrossover <- 0.85
pmutation <- 0.001
elitism_n <- 2
convergence_threshold <- 0.9 #Condición de convergencia del 90%

#FUNCIÓN DE APTITUD (fitness)
evalFunc <- function(x){
  df <- df_item_long[x == 1, ]
  total_weight <- sum(df$weight)
  total_benefit <- sum(df$benefit)
  
  if (total_weight > weightlimit) {
    #Penalización proporcional al exceso
    penalty <- 10 * (total_weight - weightlimit)
    return(total_benefit - penalty)
  }
  return(total_benefit)
}

initialize_population <- function(pop_size, n_bits) {
  matrix(sample(0:1, pop_size * n_bits, replace = TRUE), ncol = n_bits)
}

compute_fitness <- function(pop) {
  apply(pop, 1, evalFunc)
}

select_group <- function(fitness) {
  n <- length(fitness)
  idx <- order(fitness, decreasing = TRUE)
  g1 <- idx[1:floor(n * 0.25)]
  g2 <- idx[(floor(n * 0.25) + 1):floor(n * 0.5)]
  g3 <- idx[(floor(n * 0.5) + 1):floor(n * 0.75)]
  g4 <- idx[(floor(n * 0.75) + 1):n]
  
  u <- runif(1)
  if (u <= 0.5) sample(g1, 1)
  else if (u <= 0.8) sample(g2, 1)
  else if (u <= 0.95) sample(g3, 1)
  else sample(g4, 1)
}

crossover <- function(parent1, parent2) {
  point <- sample(2:(length(parent1)-1), 1)
  child1 <- c(parent1[1:point], parent2[(point+1):length(parent2)])
  child2 <- c(parent2[1:point], parent1[(point+1):length(parent1)])
  rbind(child1, child2)
}

mutate <- function(chrom, pmut) {
  mask <- runif(length(chrom)) < pmut
  chrom[mask] <- 1 - chrom[mask]
  chrom
}


#Bucle principal del GA
population <- initialize_population(pop_size, n_items)
fitness_history <- c()
valid_prop <- c()

for (gen in 1:max_gen) {
  fitness_vals <- compute_fitness(population)
  fitness_history <- c(fitness_history, max(fitness_vals))
  
  valid_count <- sum(apply(population, 1, function(ch) sum(df_item_long$weight[ch == 1]) <= weightlimit))
  valid_prop <- c(valid_prop, valid_count / pop_size)
  
  #Condición de convergencia
  if (max(table(fitness_vals)) / length(fitness_vals) >= convergence_threshold) {
    message(sprintf("Convergencia alcanzada en generación %d", gen))
    break
  }
  
  new_pop <- matrix(0, nrow = pop_size, ncol = n_items)
  
  #Elitismo
  elite_idx <- order(fitness_vals, decreasing = TRUE)[1:elitism_n]
  new_pop[1:elitism_n, ] <- population[elite_idx, ]
  
  #Resto por seleccion + cruce + mutacion
  i <- elitism_n + 1
  while (i <= pop_size) {
    p1 <- population[select_group(fitness_vals), ]
    p2 <- population[select_group(fitness_vals), ]
    
    children <- if (runif(1) < pcrossover) crossover(p1, p2) else rbind(p1, p2)
    
    children[1, ] <- mutate(children[1, ], pmutation)
    if (i <= pop_size) new_pop[i, ] <- children[1, ]
    i <- i + 1
    if (i <= pop_size) {
      children[2, ] <- mutate(children[2, ], pmutation)
      new_pop[i, ] <- children[2, ]
      i <- i + 1
    }
  }
  
  population <- new_pop
}


best_idx <- which.max(compute_fitness(population))
best_chrom <- population[best_idx, ]
selected_items <- df_item_long[best_chrom == 1, ]

total_weight <- sum(selected_items$weight)
total_benefit <- sum(selected_items$benefit)

print(selected_items)
total_weight
total_benefit


df_fit <- data.frame(Generacion = 1:length(fitness_history), BeneficioMax = fitness_history)
ggplot(df_fit, aes(x = Generacion, y = BeneficioMax)) +
  geom_line(color = "darkgreen") +
  labs(title = "Convergencia del AG", x = "Generación", y = "Beneficio máximo") +
  theme_minimal()

df_val <- data.frame(Generacion = 1:length(valid_prop), ProporcionValidos = valid_prop)
ggplot(df_val, aes(x = Generacion, y = ProporcionValidos)) +
  geom_line(color = "blue") +
  labs(title = "% de soluciones válidas por generación", y = "Proporción", x = "Generación") +
  theme_minimal()



# ==== Ejemplo de la vida real ====
#Usaré de ejemplo un dilema en el que un día me vi envuelto al decidir qué cosas 
#serían fundamentales en mi mochila de deporte. Para el peso me basé en el espacio
#que ocupan en mi mochila y no en su peso en kg.


library(dplyr)
library(ggplot2)

df_item_long <- data.frame(
  item = c("Guantes de boxeo", "Manoplas", "Vendas", "Bucal", "Botas de boxeo", "Botella de agua", "Toalla", "Vaselina", "Careta", "Desodorante", "Dulce"),
  weight = c(8, 9, 1, 1, 6, 4, 3, 2, 8, 1, 1),
  benefit = c(10, 5, 10, 9, 6, 10, 3, 1, 8, 8, 7)
)

weightlimit <- 16
n_items <- nrow(df_item_long)

# Parámetros del GA
pop_size <- 200
max_gen <- 100
pcrossover <- 0.85
pmutation <- 0.001
elitism_n <- 2
convergence_threshold <- 0.9


#Función fitness
evalFunc <- function(x){
  df <- df_item_long[x == 1, ]
  total_weight <- sum(df$weight)
  total_benefit <- sum(df$benefit)
  
  if (total_weight > weightlimit) {
    penalty <- 10 * (total_weight - weightlimit)
    return(total_benefit - penalty)
  }
  return(total_benefit)
}

initialize_population <- function(pop_size, n_bits) {
  matrix(sample(0:1, pop_size * n_bits, replace = TRUE), ncol = n_bits)
}

compute_fitness <- function(pop) {
  apply(pop, 1, evalFunc)
}

select_group <- function(fitness) {
  n <- length(fitness)
  idx <- order(fitness, decreasing = TRUE)
  g1 <- idx[1:floor(n * 0.25)]
  g2 <- idx[(floor(n * 0.25) + 1):floor(n * 0.5)]
  g3 <- idx[(floor(n * 0.5) + 1):floor(n * 0.75)]
  g4 <- idx[(floor(n * 0.75) + 1):n]
  
  u <- runif(1)
  if (u <= 0.5) sample(g1, 1)
  else if (u <= 0.8) sample(g2, 1)
  else if (u <= 0.95) sample(g3, 1)
  else sample(g4, 1)
}

crossover <- function(parent1, parent2) {
  point <- sample(2:(length(parent1)-1), 1)
  child1 <- c(parent1[1:point], parent2[(point+1):length(parent2)])
  child2 <- c(parent2[1:point], parent1[(point+1):length(parent1)])
  rbind(child1, child2)
}

mutate <- function(chrom, pmut) {
  mask <- runif(length(chrom)) < pmut
  chrom[mask] <- 1 - chrom[mask]
  chrom
}


#Bucle principal
population <- initialize_population(pop_size, n_items)
fitness_history <- c()
valid_prop <- c()

for (gen in 1:max_gen) {
  fitness_vals <- compute_fitness(population)
  fitness_history <- c(fitness_history, max(fitness_vals))
  
  valid_count <- sum(apply(population, 1, function(ch) sum(df_item_long$weight[ch == 1]) <= weightlimit))
  valid_prop <- c(valid_prop, valid_count / pop_size)
  
  if (max(table(fitness_vals)) / length(fitness_vals) >= convergence_threshold) {
    message(sprintf("Convergencia alcanzada en generación %d", gen))
    break
  }
  
  new_pop <- matrix(0, nrow = pop_size, ncol = n_items)
  
  elite_idx <- order(fitness_vals, decreasing = TRUE)[1:elitism_n]
  new_pop[1:elitism_n, ] <- population[elite_idx, ]
  
  i <- elitism_n + 1
  while (i <= pop_size) {
    p1 <- population[select_group(fitness_vals), ]
    p2 <- population[select_group(fitness_vals), ]
    
    children <- if (runif(1) < pcrossover) crossover(p1, p2) else rbind(p1, p2)
    
    children[1, ] <- mutate(children[1, ], pmutation)
    if (i <= pop_size) new_pop[i, ] <- children[1, ]
    i <- i + 1
    if (i <= pop_size) {
      children[2, ] <- mutate(children[2, ], pmutation)
      new_pop[i, ] <- children[2, ]
      i <- i + 1
    }
  }
  
  population <- new_pop
}


#Resultados
best_idx <- which.max(compute_fitness(population))
best_chrom <- population[best_idx, ]
selected_items <- df_item_long[best_chrom == 1, ]

total_weight <- sum(selected_items$weight)
total_benefit <- sum(selected_items$benefit)

print(selected_items)
total_weight
total_benefit


df_fit <- data.frame(Generacion = 1:length(fitness_history), BeneficioMax = fitness_history)
ggplot(df_fit, aes(x = Generacion, y = BeneficioMax)) +
  geom_line(color = "darkgreen") +
  labs(title = "Convergencia del AG", x = "Generación", y = "Beneficio máximo") +
  theme_minimal()

df_val <- data.frame(Generacion = 1:length(valid_prop), ProporcionValidos = valid_prop)
ggplot(df_val, aes(x = Generacion, y = ProporcionValidos)) +
  geom_line(color = "blue") +
  labs(title = "% de soluciones válidas por generación", y = "Proporción", x = "Generación") +
  theme_minimal()




#xd