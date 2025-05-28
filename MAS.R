####Muestreo Aleatorio simple 
library("Rlab")
#Estimacion de M poblacional ************************************************
N <-  120#elementos de la población
n <-  12#elementos que pertenecen a la poblacion
val <- c(35.5,30.2,28.9,36.4,29.8,34.1,32.6,26.4,38,38.2,32.2,27.5)
s <-  var(val)#varianza muestral 
v <-  NA#varianza de la poblaion (si no se conoce introducior NA)
yi <- sum(val)#sumatoria de los elementos 


y<- yi/n 
if (is.na(v)){
  vary <- (s/n)*((N-n)/N)
}else{
  vary <- (v/n)*((N-n)/N-1)
}
B <- 2*sqrt(vary)
ME <- (B/y)*100
ici <- y-B
ics <- y+B

#Estimación de un t poblacional*********************************************
N <-  120#elementos de la población
n <-  12#elementos que pertenecen a la poblacion
val <- c(35.5,30.2,28.9,36.4,29.8,34.1,32.6,26.4,38,38.2,32.2,27.5)#valores del muestro 
s <-  var(val)#varianza muestral 
v <-  NA#varianza de la poblaion (si no se conoce introducior NA)
yi <- sum(val)#sumatoria de los elementos 

y <- yi/n
t<- y*N
if (is.na(v)){
  vary <- (N*s/n)*(N-n)
}else{
  vary <- N^2*((v/n)*((N-n)/N-1))
}
B <- 2*sqrt(vary)
ME <- (B/t)*100
ici <- t-B #Intervalo de confianza inferior 
ics <- t+B#Intervalo de confianza superior 
#Estimación de una p poblacional*********************************************
N <-  120#elementos de la población
n <-  12#elementos que pertenecen a la poblacion
yie <- 7#sumatoria de exitos 
v <-  NA #NA solo si la varianza es desconocida 
p<- yie/n

if (is.na(v)){
  vary <- ((p*(1-p))/n)*((N-n)/N)
}else{
  vary <- ((p*(1-p))/n)*((N-n)/(N-1))
}
B <- 2*sqrt(vary)
ME <- (B/p)*100
ici <- p-B
ics <- p+B

#Estimación de muestra n ****************************************************
N <- 500#tamaño poblacional
B <- .03#beta 
v <- .5*(1-.5)#Varianza (rango de variacion r^2/16, p(1-p))
#TOME EL 50% YA QUE SE PUEDE TOMAR DOS OPCIONES 1 "ACEPTADO" O 2 "NO ACEPTADO"
tn <- 'P' # n Para M media, T total y P proporción
if ((tn == 'M') | (tn == 'P')){
  D <- B^2/4
}else{
  D <- B^2/4*N^2
}
n <- (N*v)/(((N-1)*D)+v)
K <- N/n
