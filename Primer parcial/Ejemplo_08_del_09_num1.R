"Escriba un programa con funciones realice una calculadora a traves de funciones vacias"

principal <- function(){
  calculadora()
}


calculadora <- function(){
  cat("Ingresa el operando 1 y 2 separados por un enter\n")
  operando <- readLines(n = 2)
  operando <- as.numeric(operando)
  cat ("Ingresa la operacion a realizar\n
       +: suma\n-: Resta\n*: Multiplicación\n/:División\n")
  
  oper <- readLines(n = 1)
  oper <- as.character(oper)
  
  res <- switch(oper,
                '+' = sum (operando),
                '-' = operando [1] - operando[2],
                '*' = operando [1] * operando[2],
                '/' = operando [1 / operando[2]],
                )
  cat("El resultado de ", operando [1], oper, operando[2], " = ", res)
  
}