"1.-
  words es una función que devuelve más de 900 palabras en inglés. 

Encuentra aquellas palabras que empiecen con una vocal
Encuentra aquellas palabras que terminen con day
Encuentra aquellas palabras que empiecen y terminen con el mismo carácter"

main <- function(){
  lista = words
  conteo (lista)
}

conteo <- function(lista){
  inincioVocal <- str_view(lista, "^[aeiouAEIOU]")
  terminaDay <- str_view(lista, "day$")
  inicioYFinal <- str_view(lista, "^aeiouAEIOU$")
  print("Las palabras que inician con vocal son:\n",inincioVocal, "\nLas que terminan con day son:\n",terminaDay, "\nLas que incian y terminan con vocal son: \n",inicioYFinal)
  
}
main()