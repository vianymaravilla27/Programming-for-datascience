"Realice una funcion que lea una cadena y en esta indentifique el 
numero ordinal y lo cambie por cardinal"

principal <- function(){
  cadena <- solicitaCad
  cambiaNum(cadena)
}

solicitaCad <- function ()
  cad = readline("Ingresa el numero ordinal")

cambiaNum <- function(cadena){
  cad <- str_to_lower(cadena)
  
  cad <- str_replace(cadena, "primer", "1")
  
  cat("La cadena con el reemplazo es ", cad)
}

principal()