"Escribe una función que reciba un vector 
de números y devuelva una lista con los elementos positivos y los elementos negativos del vector."

postitivos_negativos <- function(vc){
  positivos <- vc[vc > 0]
  negativos <- vc[vc < 0]
  
  cat("Numeros postivos:",positivos,"\nNegativos:", negativos)
}

leer_vector <- function(){
  cat("Ingrese los numeros deseados")
  vc = scan()
  postitivos_negativos(vc)
}