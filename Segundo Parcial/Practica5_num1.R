"Los siguientes datos re´resentan los pesos en ggramos de contenido
de 16 cajas de cereal que se seleccionaron al azar de un proceso de 
verificar medidas de tendenci central, posición (cuartiles y deciles)
así como, de dispersión obtengalas e interprete"

library(modeest)

main <- function(){
  #Funcion principal que manda llamar a las demas para resolver el problema
  
  dat <- c(506,508,499,503,504,510,497,512,514,505,493,496,506,502,509,496)
  dat <- sort(dat, decreasing = FALSE)
  posi_cartiles <- cuartiles(dat)
  cart <- c(dat[posi_cartiles[1]], dat[posi_cartiles[2]], dat[posi_cartiles[3]])
  mediana <- 498.50
  maximo <- Rango(dat)
  var <- Varianza(dat, mediana)
  desvi <- Desviacion(dat, mediana)
  mod <- mode(dat)
  #deciles <- Deciles(dat)
  
  #Se muestran los datos imprimiendolos uno a uno ya que al guardarlos en un data frame, da un error porque no son de la misma longitud
  cat("Datos:\n",dat)
  cat ("\nCuartiles:\n",cart)
  cat("\nMediana:\n", mediana)
  cat("\nModa:\n", mod)
  cat("\nEl rango:\n",maximo)
  cat ("\nVarianza:\n",var)
  cat ("\nDesviacion Estandar:\n", desvi)
  #cat("\nDeciles:\n", deciles)
  
  cat("\t\t\nLas que calcula R con las funciones integradas:\n")
  cat("\nMediana:\n", median(dat))
  cat("\nVarianza:\n", var(dat))
  cat("\nModa:\n",mfv(dat))
  cat("\nCuartiles:\n",quantile(dat))
  
  cat("\n\t\tInterpretación:\n")
  cat("\n\tCuartiles:")
  cat("\nEl 25% de las cajas tiene al menos:",cart[1])
  cat("\nEl 50% de las cajas tiene al menos:",cart[2])
  cat("\nEl 75% de las cajas tiene al menos:",cart[3])
  
  #cat("\n\tDeciles:")
  #cat("\nEl 10% de las cajas tiene al menos:",deciles[1])
  #cat("\nEl 20% de las cajas tiene al menos:",deciles[2])
  #cat("\nEl 30% de las cajas tiene al menos:",deciles[3])
  #cat("\nEl 40% de las cajas tiene al menos:",deciles[4])
  #cat("\nEl 50% de las cajas tiene al menos:",deciles[5])
  #cat("\nEl 60% de las cajas tiene al menos:",deciles[6])
  #cat("\nEl 70% de las cajas tiene al menos:",deciles[7])
  #cat("\nEl 80% de las cajas tiene al menos:",deciles[8])
  #cat("\nEl 90% de las cajas tiene al menos:",deciles[9])
}

cuartiles <- function (dat){
  #funcion para calcular los 3 cuartiles 
  n <- length(dat)
    q2 <- (2*n)/4
    q1 <- (1*n)/4
    q3 <- (3*n)/4
  
  cuartiles <- c(q1, q2, q3)
  
  return (cuartiles)
}

Rango <- function (dat){
  maximo <- max(dat)
  minimo <- min(dat)
  R <- maximo - minimo
  return (R)
}

Varianza <- function(dat, mediana){
  n <- length(dat)
  varia <- numeric()
  for (i in length(dat)){
    varia <- c(varia, ((dat[i]-mediana)/n))
  }
  
  return(varia)
}

Desviacion <- function(dat,mediana){
  n <- length(dat)
  desv <- numeric ()
  for (i in length(dat)){
    d <- (dat[i]-mediana)/n
    desv <- c(desv,(sqrt(d)))
  }
  
  return (desv)
}

mode <- function(dat) {
  return(as.numeric(names(which.max(table(dat)))))
}

Deciles <- function (dat){
  n <- length(dat)
  i <- 1
  decil <- numeric()
  while(i <= 10){
    dec <- (i*n)/10
    decil <- c(decil, dec)
    i = i + 1
  }
  
  return(decil)
}

main()