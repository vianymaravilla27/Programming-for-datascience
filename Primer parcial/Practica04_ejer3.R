codigovuel <- integer()
num_pasaj <- integer()
linea_aerea <- character()
hora_llega <- character()
Origen_sal <- character()
Destino <- character()

for( i in 1:3){
  cat("Ingresa el codigo de vuelo:")
  cod <- readLines(n=1)
  cod <- as.integer(cod)
  codigovuel <- c(codigovuel,cod)
  
  cat("Ingresa el numero de pasajero:")
  numpa <- readLines(n=1)
  numpa <- as.integer(numpa)
  num_pasaj <- c(num_pasaj, numpa)
  
  cat("Ingresa la aerolinea:")
  aera <- readLines(n=1)
  linea_aerea <- c(linea_aerea,aera)
  
  cat("Ingresa la hora de llegada")
  horall <- readLines(n=1)
  hora_llega <- c(hora_llega,horall)
  
  cat("Ingresa de donde partes:")
  sal <- readLines(n=1)
  Origen_sal <- c(Origen_sal, sal)
  
  cat("Ingresa tu destino:")
  dest <- readLines(n=1)
  Destino <- c(Destino,dest)
}
tabe <- data.frame(codigovuel, num_pasaj, linea_aerea, hora_llega, Origen_sal, Destino)
cat("Se procede a ordenarla de forma descendente y con respecto al código de vuelo:")
tabe[order(codigovuel)]
print(tabe)
anexo <- list(908, 85, "Volaris", "11:30", "Tijuana", "Cd. Mexico")
tabe <- rbind(tabe,anexo)
print (tabe)
cat("Se procede a cambiar a 'Volaris' por 'TAR'")
tabe[[4,3]] <- "TAR"
print (tabe)

