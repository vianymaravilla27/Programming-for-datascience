"
Realice el script que resuelva el siguiente problema. Almacene en un vector las temperaturas de un día (0-23)
Culcule su media e imprima la temperatura mas alta y la mas baja; asi mismo imprimir la hora respectiva. Por
ejemplo la temperatura media fue de 21.5, la mas alta de 29-14hrs y la mas baja de 9-3hrs.
"
temperatura <- numeric()
horas <- character() 

for (i in 1:24) {
  cat('Ingrese la temperatuta actual')
  temp <- readLines(n=1)
  temp <- as.numeric(temp)
  temperatura <- c(temperatura, temp)
  cat('Ingrese a que hora se registro la temperatura anterior')
  hr <- readLines(n=1)
  horas <- c(horas, hr)
}
alta <- which.max(temperatura)
baja <- which.min(temperatura)
cat('La media de las temperaturas fue: ', mean(temperatura))
cat("la temperatura mas alta registrada fue de: ",which.max(temperatura)," a las ",horas [alta])
cat("La teemperatura mas baja fue de: ", which.min(temperatura), " a las: ", horas[baja])
