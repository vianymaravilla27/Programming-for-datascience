"Escribe una función que dado un valor y un vector
devuelva cuántas veces se encuentra el valor en el
vector."

Conteo <- function(cv){
  num_cont <- 0
  nuum <- readline("Ingresa que numero para que se te muestre la cantidad de veces que esta guardado\n")
  nuum <- as.integer(nuum)
  for (i in (1:length(cv))){
    if (cv[i] == nuum){
      num_cont = num_cont + 1
    }
  }
  cat("El numero: ", nuum , "se encuentra: ", num_cont, "veces.")
  
}

Principal <- function (){
  cat("Ingresa los numeros que desees:\n")
  cv <- scan()
  Conteo(cv)

}
