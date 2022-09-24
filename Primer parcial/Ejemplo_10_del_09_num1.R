"Realice un script que implemente una calculadora con funciones utilizando como argumentos
la lista del operador y los operandos"

principal <- function(){
  datos <- solicitaLista()
  calculadora(oper = datos[[2]], operando2 = datos[[1]][2], operando1 = datos[[1]][1]) #Invoco funcion con parametros
}

solicitaLista <- function(){
  cat("Ingresa el operando 1 y 2 separados por un enter\n")
  operando <- readLines(n = 2)
  operando <- as.numeric(operando)
  
  cat ("Ingresa la operacion a realizar\n+: suma\n-: Resta\n*: MultiplicaciÃ³n\n/:DivisiÃ³n\n")
  oper <- readLines(n = 1)
  oper <- as.character(oper)
  
  datos <- list(operando, oper)
}

calculadora <- function(operando1, operando2, oper){
  
  
  res <- switch(oper,
                '+' = operando1 + operando2,
                '-' = operando1 - operando2,
                '*' = operando1 * operando2,
                '/' = operando1 / operando2,
  )
  cat("El resultado de ", operando1, oper, operando2, " = ", res)
  
}