"Una matriz es simétrica si es cuadrada y es igual a
su traspuesta. Escribe una función que devuelva un
valor lógico indicando si una matriz es simétrica."

Comprobar <- function (A,trans_mat){
  contsi <- 0
  for (i in (1:length(A))){
    if (trans_mat[[i]] == A[[i]]){
      contsi = contsi + 1
    }
  }
  if (contsi == (length(A)))return(TRUE) else return(FALSE)
}
Principal <- function(){
  A <- matrix(16:31,nrow = 4, ncol = 4, byrow = TRUE)
  trans_mat <- t(A)
  if (Comprobar(A,trans_mat) == TRUE) {
    cat("La matriz es simetrica\n")
    print (A)
    cat("\n")
    print(trans_mat)
  }else {
    cat("La matriz no es aimetrica\n")
    print (A)
    cat("\n")
    print(trans_mat)
  }
}
