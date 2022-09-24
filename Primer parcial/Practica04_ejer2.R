"
Almacene los elementos de una matriz m x n (5 x 5) imprimir cuantos números son ceros, cuantos son negativos y cuantos positivos. 
Imprimir además la suma de los negativos, positivos y la diagonal 
"num_uno <- integer()
num_dos <- integer ()
num_tres <- integer()
num_cuatro <- integer()
num_cinco <- integer()

for(i in 1:5){
  cat("Ingresa un numero:\n")
  num1 <- readLines(n=1)
  num1 <- as.integer(num1)
  num_uno<- c(num_uno,num1)
  
  cat("Ingresa un numero:\n")
  num2 <- readLines(n=1)
  num2 <- as.integer(num2)
  num_dos<- c(num_dos,num2)
  
  cat("Ingresa un numero:\n")
  num3 <- readLines(n=1)
  num3 <- as.integer(num3)
  num_tres<- c(num_tres,num3)
  
  cat("Ingresa un numero:\n")
  num4 <- readLines(n=1)
  num4 <- as.integer(num4)
  num_cuatro<- c(num_cuatro,num4)
  
  cat("Ingresa un numero:\n")
  num5 <- readLines(n=1)
  num5 <- as.integer(num5)
  num_cinco<- c(num_cinco,num5)
  
  mati <- matrix(rbind(num_uno,num_dos,num_tres,num_cuatro,num_cinco),nrow = 5,ncol = 5)
  
}
contpos <- 0
sumapos <- 0
contneg <- 0
sumaneg <- 0
contcer <- 0
sumdiag <- sum(diag(mat))
ifelse(mati>0, contpos <- contpos + 1,contpos <- contpos + 0 )
ifelse(mati>0, sumapos <- sumapos + 1,sumapos <- sumapos + 0 )
ifelse(mati<0, contneg <- contneg + 1,contneg <- contneg + 0 )
ifelse(mati<0, sumaneg <- sumaneg + 1,sumaneg <- sumaneg + 0 )
cat("La cantidad de numeros positivos es: ", contpos, " y su suma es: ", sumapos)
cat("La cantidad de numeros negativos es: ", contneg, " y su suma es: ", sumaneg)
cat("La suma de la diagonal es:", sumdiag)



