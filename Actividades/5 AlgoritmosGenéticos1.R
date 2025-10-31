#Algoritmos genéticos 1

# ==== Un pequeño tour de los algoritmos genéticos ====
library(GA)

f<-function(x) (x^2+x)*cos(x)
lbound<--10;unbound<-10
curve(f, from = lbound, to=unbound, n=1000)
GA<-ga(type="real-valued", fitness=f, lower=c(th=lbound), upper = unbound)
summary(GA)
plot(GA) #evolución del algoritmo respecto al valor de la función de actitud contra el número de generaciones

curve(f, from=lbound, to=unbound, n=1000)
points(GA@solution, GA@fitnessValue, col=2, pch=19) #máximo global de la función

# ---> Optimización de funciones en dos dimensiones
Rastrigin <- function(x1, x2){
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2)) 
} 

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1) 
f <- outer(x1, x2, Rastrigin) 
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors) 

#Búsqueda de minimización de GA
GA<-ga(type = "real-valued",
       fitness = function(x) -Rastrigin(x[1],x[2]),
       lower = c(-5.12,-5.12), upper = c(5.12, 5.12),
       popSize = 50, maxiter = 1000, run=100)

summary(GA)
plot(GA)

filled.contour(x1,x2,f,color.palette = bl2gr.colors,
               plot.axes = {axis(1);axis(2);
                 points(GA@solution[,1],GA@solution[,2],
                        pch=3, cex=2, col="white", lwd=2)}
               )

#Proceso de búsqueda del AG 
monitor<-function(obj){
  contour(x1,x2,f,drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration=", obj@iter), front.main=1)
  points(obj@population, pch=20, col=2)
  Sys.sleep(0.2)
}

GA<-ga(type = "real-valued",
       fitness = function(x)-Rastrigin(x[1],x[2]),
       lower = c(-5.12,-5.12), upper = c(5.12,5.12),
       popSize = 50, maxiter = 100,
       monitor = monitor)


#Configuración de algunos miembros de la población inicial
suggestedSol<-matrix(c(0.2,1.5,-1.5,0.5),nrow = 2, ncol = 2, byrow = TRUE)

GA1 <- ga(type = "real-valued",
          fitness =  function(x) -Rastrigin(x[1], x[2]), 
          lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
          suggestions = suggestedSol, 
          popSize = 50, maxiter = 1) 

head(GA1@population)

GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]), 
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         suggestions = suggestedSol, 
         popSize = 50, maxiter = 100) 

summary(GA)



# ==== Algoritmo genético para resolver el problema de la mochila ====

evalFunc <- function(x){
  df <- df_item_long[x == 1, ]
  total_weight <- sum(df$weight)
  
  # Penalización más fuerte si excede el peso límite
  if (total_weight > weightlimit) {
    return(-500)
  }
  
  return(total_weight)
}

tic()
gann_mod <- ga(
  type = "binary", 
  fitness = evalFunc, 
  popSize = 200,        # Aumentado de 100 → 200
  maxiter = 300,        # Aumentado de 100 → 300
  run = 20, 
  nBits = nrow(df_item_long), 
  seed = 123,
  pmutation = 0.2,      # Aumentado de 0.1 → 0.2 (más mutación)
  pcrossover = 0.9      # Aumentado de 0.8 → 0.9 (más combinaciones)
) 
toc()

summary(gann_mod)
plot(gann_mod)

df_sol <- df_item_long[gann_mod@solution[1,] == 1,]
df_sol <- df_sol %>%
  group_by(item, weight) %>%
  summarise(freq = n()) %>%
  mutate(total_weight = freq * weight)

df_sol
sum(df_sol$total_weight)






#xd