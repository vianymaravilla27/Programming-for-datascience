'''Calcular la hipotenusa de un triangulo
'''

repetir <- "si"
while(repetir == "si" || repetir == 'Si' || repetir =="s" || repetir == "S"){
  cat('Ingresa el cateto adyacente')
  cateto_ady <- readLines(n=1)
  cateto_ady <- as.numeric(cateto_ady)
  cat('Ingresa el cateto opuesto')
  cateto_opuesto <- readLines(n=1)
  cateto_opuesto <- as.numeric(cateto_opuesto)
  hipotenusa <- sqrt((cateto_ady**2) + (cateto_opuesto**2))
  cat("La hipotenusa es: ", hipotenusa, "\n")
  cat("¿Quieres volver a calcularla?")
  repetir <- readLines(n=1)
}