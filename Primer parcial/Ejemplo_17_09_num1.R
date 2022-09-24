"colors() es una función que devuelve el nombre de más de 600 colores en R. 
Encuentra aquellos cuyo nombre contenga un número
Encuentra aquellos que comiencen con yellow 
Encuentra aquellos que contengan blue 
Reemplaza secuencias de números por x (por ejemplo, blue10 quedaría como bluex) "

Inicio <- function(){
  colores <- colors()
  coloresY <- encuentraColor(colores, "yellow")
  cat ("Los colores amarillos son:\n", coloresY)
  coloresB <- encuentraColor(colores, "blue")
  cat ("\nLos colores azules son:\n", coloresB)
  reemplazaNum(coloresB)
  nombreCNum(colores)
  
}

encuentraColor <- function (colores, color){
  patron <- paste("^", color, sep = "")
  coloresY <- grep(patron, colores, value = TRUE)
}

reemplazaNum <- function (colores){
  colores<- gsub("[0 - 9]$", "x", colores)
  cat("\nLos colores con reemplazo son: ", colores)
}

nombreCNum <- function (colores){
  coloresNum <- grep("[0 - 9]$", colores, value = T)
  cat ("\nLos colores con numeros son:\n")
  print(coloresNum)
}
Inicio ()
