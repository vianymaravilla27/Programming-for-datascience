"Realice un script que implemente una calculadora con funciones utilizando como argumentos
la lista del operador y los operandos"

principal <- function(){
  listaDatos <- solicitaLista()
  calculadora(listaDatos) #Invoco funcion con parametros
}

solicitaLista <- function(){
  cat("Ingresa el operando 1 y 2 separados por un enter\n")
  operando <- readLines(n = 2)
  operando <- as.numeric(operando)
  
  cat ("Ingresa la operacion a realizar\n+: suma\n-: Resta\n*: Multiplicación\n/:División\n")
  oper <- readLines(n = 1)
  oper <- as.character(oper)
  
  datos <- list(operando, oper)
}

calculadora <- function(listaDatos){

  
  res <- switch(listaDatos[[2]],
                '+' = sum (listaDatos[[1]]),
                '-' = listaDatos [[1]][[1]] - listaDatos [[1]][[2]],
                '*' = listaDatos [[1]][[1]] * listaDatos [[1]][[2]],
                '/' = listaDatos [[1]][[1]] / listaDatos [[1]][[2]],
  )
  cat("El resultado de ", listaDatos [[1]][[1]], listaDatos[[2]], listaDatos [[1]][[2]], " = ", res)
  
}