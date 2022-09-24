"Lea una cadena e imprimir el total de vocales, 
dígitos, caracteres especiales, mayúsculas, minúsculas, 
espacios en blanco, consonantes y total de los caracteres de la cadena"

Inicio <- function (){
  cadena = readline("Ingresa una cadena\n")
  analizacadena(cadena)
}

analizacadena <- function (cadena){
  print("Resumen de la cadena\n
      \nVocales -",str_count(cadena, "[aeiouAEIOU]"),
      "\nMayusculas -", str_count(cadena, "[:upper:]"),
      "\nDigitos - ", str_count(cadena, "[:digit:]"),
      "\nMinusculas-", str_count(cadena, "[:lower:]"),
      "\nCaracteres especiales -", str_count(cadena, "[:punct:]"),
      )
}

Inicio()