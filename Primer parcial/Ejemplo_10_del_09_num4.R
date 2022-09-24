"Cree una funcion que devuelva el valorabsoluto de un numero sin usar la funcion abs"

uno <- function(){
  num <- readline("Ingrese un numero:\n")
  num <- as.integer(num)
  cat("El valor absoluto es: ", My_abs(num))
}
My_abs <- function(num){
  if (num < 0) return (num * -1) else return (num) 
}