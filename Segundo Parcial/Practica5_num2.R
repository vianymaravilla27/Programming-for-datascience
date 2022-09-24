"La siguiente tabla muestra la cantidad de libros
vendidos en una librería y su frecuencia. Calcule las
medidas de tendencia central, posicion (cuartiles);
así como de dispercion e interprete
intervalos  | frecuencia
0-4           3
4-8           5
8-12          6
12-16         4
16-20         3
"
library(stringr)

main<- function(){
  
  frec <- c(3,5,6,4,3)
  datos <- data.frame("Intervalos" = c("0-4","4-8","8-12","12-16","16-20"),
                      "Frecuencia" = frec)
  MarcaDeClase <- Marcadeclase()
  datos <- cbind(datos, MarcaDeClase)
  FrecuenciaAcum <- FrecuenciaAcumulada(datos)
  datos <- cbind (datos, FrecuenciaAcum)
  PosicionCuartiles <- cuartiles(frec)
  PosicionCuartiles <- c(PosicionCuartiles, NA)
  datos <- cbind(datos, PosicionCuartiles)
  valCuart <- ValorCuart(frec,PosicionCuartiles)
  valCuart <- c(valCuart, NA)
  datos <- cbind(datos, valCuart)
  datos
  #SalvarArchivo(datos)
}

cuartiles <- function (frec){
  n <- length(frec)
  pos1 =1*((n+1)/4)
  pos1 = round(pos1, 0)
  pos2 =2*((n+1)/4)
  pos2 = round(pos2, 0)
  pos3 =3*((n+1)/4)
  pos3 = round(pos3, 0)
  pos4 =4*((n+1)/4)
  pos4 = round(pos4, 0)
  PosC <- c(pos1, pos2, pos3, pos4)
  return(PosC)
  
}

Marcadeclase <- function(){
  mar1 <- (0+4)/2
  mar2 <- (4+8)/2
  mar3 <- (8+12)/2
  mar4 <- (12+16)/2
  mar5 <- (16+12)/2
  marcClase <- c(mar1, mar2, mar3, mar4,mar5)
  return (marcClase)
}

FrecuenciaAcumulada<- function(datos){
  i <- 1
  j <- 0
  fre <- c(3)
  while (i < 6){
    fre <- c(fre, (datos$Frecuencia[i]+datos$Frecuencia[(j)]))
    i = i + 1
    j = j + 1
  }
  return (fre)
}

ValorCuart <- function (frec, PosicionCuartiles) {
  N = sum(frec)
  q1 <- 0 + ((PosicionCuartiles[1]*N/4)-0)/3 * 4
  q2 <- 4 + ((PosicionCuartiles[2]*N/4)-3)/5 * 4
  q3 <- 4 + ((PosicionCuartiles[3]*N/4)-3)/5 * 4
  q4 <- 4 + ((PosicionCuartiles[4]*N/4)-3)/5 * 4
  
  Valorcuart <- c(q1,q2,q3,q4)
  return(Valorcuart)
}

SalvarArchivo <- function (datos){
  ruta <- getwd()
  ruta <- stringr::str_c(ruta, "/Practica5.csv")
  write.csv(datos, ruta, sep = ",")
}

main()
