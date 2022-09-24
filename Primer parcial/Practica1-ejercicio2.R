'''Programa para calcular el IMC  de un persona, dado su masa y altura
'''
altura <- -5.2
masa <- -2.5
imc <- 0.0
while(altura <= 0){
  cat('\nIngrese su altura en metros:')
  altura <-scan(,what=numeric(),1)
  if(altura <= 0){
    cat("La opcion ingresada no es valida, vuelva a intentarlo.")
  }
}
while(masa <= 0){
  cat('\nIngrese su masa en metros:')
  masa <-scan(,what=numeric(),1)
  if(masa <= 0){
    cat("La opcion ingresada no es valida, vuelva a intentarlo.")
  }
}
imc <- masa/(altura^2)
if(imc < 18.5){
  cat('\nSu imc de', imc, " es insuficiencia ponderal")
}
if(18.5 < imc && imc < 24.5){
  cat('\nSu imc de', imc, " intervalo normal")
}
if(imc>=25){
  cat('\nSu imc de', imc, " sobrepeso")
}
if(imc >= 30){
  cat('\nSu imc de', imc, " obesidad")
}
if(30.0 < imc && imc < 34.5){
  cat('\nSu imc de', imc, " obesidad de clase 1")
}
if(35 < imc && imc < 39.5){
  cat('\nSu imc de', imc, " obesidad de clase 2")
}
if(imc >= 40){
  cat('\nSu imc de', imc, " obesidad de clase 3")
}
