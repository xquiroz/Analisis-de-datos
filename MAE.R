#Muestreo aleatorio estratificado 
install.packages("readxl")
library("readxl")
library("Rlab")
#leera un libro de excel donde estara  E el numero de estrato, Mi la media , Ni elementos 
# del estrato, ni elementos seleccionados del estrato, si varianzas 

file.choose() #te da el archivo a leer en forma en que r pueda entenderlo 
ruta <- "C:\\Users\\saund\\OneDrive\\Escritorio\\Muestreo\\extraordinario prob2.xlsx"
D <- read_excel(ruta,range ="E1:I5") #Tabla de datos 
N <- 170 #Tamaño de poblacion
V <- NA
E <- 4
#NA si se desconoce la varianza poblacional de cada estrato 
#transformamos en vecores cada una de las columnas 
Mi <- D$Mi #media 
Ni <- D$Ni #Tamaño de estratp 
ni <- D$ni #tamaño de muestra de estrato
si <- D$Si #Varianza muestral de estrato
#Estimación de una  M poblacional ***************************************
Media <- Mi*Ni #Multiplicacion de vectores 
M <- sum(Media)/N #Suma del vector entre N elementos de población

varm <- c() #Creamos vector para la var de m 
if (is.na(V)){
  for(i in 1:E){
  varm[i] <- ((Ni[i]*si[i])/ni[i])*(Ni[i]-ni[i]) 
  }
}else{
  for(i in 1:E){
    varm[i] <- (((Ni[i]^2)*si[i])/ni[i])*((Ni[i]-ni[i])/(Ni[i]-1))
  }
}
VM <- sum(varm)/N^2 #Varianza de M
B <- 2*sqrt(VM) 
ME <- (B/M)*100

#Estimacion de t poblacional *******************************************
t <- M*N
vart <- sum(varm)
B <- 2*sqrt(vart)
ME <- (B/t)*100

#Estimacion de p poblacional 
tp <- 50 #Total de la muestra de proporción 
Vp <- NA #NA si no se conoce la varianza poblacional de los estratos 
nip <- (Ni/N)*tp #nuevo ni para cada estrato 
cf <- c(16, 2, 6) #vector de los casos favorables 
ppe <- cf/nip #cf/ct
sip <- ppe*(1-ppe)

p <- sum(Ni*ppe)/310 #proporcion 

if (is.na(Vp)){
    varp <- ((Ni*sip)/nip)*(Ni-nip) 
}else{
    varp <- (((Ni^2)*sip)/nip)*((Ni-nip)/(Ni-1))
}
VP <- sum(varp)* (1/N^2)
B <- 2*sqrt(VP)
ME <- (B/p)*100
