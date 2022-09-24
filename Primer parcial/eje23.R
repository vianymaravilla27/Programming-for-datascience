"Dada una cadena con acaentos y signos de puntuacion eliminar todos los caracteres especiales"

prinicpal <- function (){
  cadena = readline("Ingresa la cadena a trabajar")
  cadena <- iconv(cadena, to ="ASCII//TRANSLIT")
  cadena <- str_replace_all(cadena, "[:punct:]", " ")
  cadena
}

prinicpal()
