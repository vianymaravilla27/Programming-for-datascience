"Realiza una versión de la función sign. Esta función recibe un vector numérico y devuelve 
un vector del mismo tamaño que el vector original, pero con los valores 1 (valores positivos), 
0 (valores 0) y -1 (valores negativos). Por ejemplo, sign(c(-4, 2, 0, 3)) 
devuelve el vector c(-1, 1, 0, 1)."

funcion <- function (){
  cat("Ingresa numeros")
  ejemplo = scan()
  vec <- My_sign(ejemplo)
  print(vec)
}

My_sign <- function(ejemplo){
  for (i in 1:length(ejemplo)){
    if (ejemplo[i] > 0){
      ejemplo[i] <- 1
    }else{
      if (ejemplo[i] < 0){
        ejemplo[i] <- -1
      }else{
        ejemplo[i] <- 0
      }
    }
  }
  return (ejemplo)
}
