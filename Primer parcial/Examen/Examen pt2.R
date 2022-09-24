"El domingo de Pascua es el primer domingo
después de la primera luna llena posterior al equinoccio de primavera y se
determina mediante el siguiente cálculo:
  
  
a = año mod19   
b = año mod 4      c = año mod 7   d=(19*a+24) mod 30


e = (2 * b + 4 * c + 6 * d + 5) mod  7   n = (22 + d + e)

Donde n indica el número del día del mes de
marzo (si n es igual o menor que 31), en caso contrario el mes de abril se
obtiene como (d+e-9). Imprimir la fecha del domingo de Pascua a partir de un
año dado"

main <- function(){
  cat("\nProgramacion para ciencia de datos\n")
  año <- readline("Ingrese un año\n")
  año <- as.integer(año)
  calculo(año)
}
calculo <- function (año){
  a = año %% 19
  b = año %% 4
  c = año %% 7
  d = (19 * a + 24) %% 30
  e = (2*b + 4*c + 6*d + 5) %% 7
  n = (22 + d + e)
  if( n <= 31){
    cat("El domingo de pascua fue:", n)
    cat("Del año:", año)
  }else {
    cat("Es en el mes de abril el:", (d+e-9))
  }
}

main()