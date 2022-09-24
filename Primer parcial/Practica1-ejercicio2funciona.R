altu<- 1.75
masa<- 70
imc <- masa/ altu^2
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
