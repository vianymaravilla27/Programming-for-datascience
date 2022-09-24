"Realize un script que implemente una calculadora utilizando como argumentos 
el operador y los operandos"

principal <- function(){
  operando <- solicitaOperando() #Retorno de funcion
  oper <- solicitaOPer()
  calculadora(operando, oper) #Invoco funcion con parametros
}

solicitaOperando <- function(){
  cat("Ingresa el operando 1 y 2 separados por un enter\n")
  operando <- readLines(n = 2)
  operando <- as.numeric(operando)
}

solicitaOPer <- function (){
  cat ("Ingresa la operacion a realizar\n+: suma\n-: Resta\n*: Multiplicación\n/:División\n")
  oper <- readLines(n = 1)
  oper <- as.character(oper)
}

calculadora <- function(operando, oper){
  res <- switch(oper,
                '+' = sum (operando),
                '-' = operando [1] - operando[2],
                '*' = operando [1] * operando[2],
                '/' = operando [1 / operando[2]],
  )
  cat("El resultado de ", operando [1], oper, operando[2], " = ", res)
  
}