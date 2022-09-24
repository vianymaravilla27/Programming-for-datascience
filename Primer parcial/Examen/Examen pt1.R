"Escriba una función llamada minimo que permita obtener el valor mínimo 
de un vector numérico.
No puede usar ninguna de las funciones básicas de R como which.min(),
 which.max(), order(), min( ), max( ), sort( ) u order( )."

main <- function (){
  cat("\nProgramación para ciencia de datos\n")
  cat("Ingrese los numeros que dessee:\n")
  vec <- scan()
  MaxyMin (vec)
}
MaxyMin <- function (vec){
  j <-2
  for (i in length(vec)){
    if (vec[i] > vec[(j)]){
      maximo <- vec[i]
    }
    j = j + 1
  }
  j <-2
  for (i in length(vec)){
    if (vec[i] > vec[j]){
      minim <- vec[i]
    }
    j = j + 1
  }
  cat("EL máximo número del vector dado es:", maximo)
  cat("\nEl minimo es:", minim)
}
main()