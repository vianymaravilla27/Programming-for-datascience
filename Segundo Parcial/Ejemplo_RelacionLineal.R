#Regresion lineal ejemplo 1 en clase

"Una cadena de Pizzerías toma una muestra de diez de sus 
sucursales para tratar de encontrar un modelo matemático 
que le permita predecir sus ventas y obtuvo los siguientes datos: 
la población de personas en miles fue de 2, 6, 8, 8, 12, 16, 20, 20, 22, 26; 
y las ventas trimestrales en miles de pesos fue de: 58, 105, 88, 118, 117, 137, 157, 168, 149, 202. 
Realice una regresión para estimar las ventas de dos sucursales que tienen 14,000 y 30,000 personas 
como potenciales clientes respectivamen"
library(ggplot2)
library(modeest)

main <- function (){
  personas <- c(2, 6, 8, 8, 12, 16, 20, 20, 22, 26)
  ventas <- c(58, 10, 88, 118, 117, 137, 157, 168, 149, 202)
  
  graf1 <- ggplot(data = NULL, aes(personas,ventas)) + geom_point()
  
  #Producto de x*y 
  xy <- numeric()
  i <- 1 
  while (i <= length(personas) && i <= length(ventas)){
    xY <- personas[i] * ventas[i]
    xy <- c(xy,xY)
    i = i + 1
  }
  
  #Obtencion de x^2
  i <- 1 
  x2 <- numeric ()
  while (i <= length(personas)){
    cuad <- personas[i] * personas[i]
    x2 <- c(x2, cuad)
    i = i + 1
  }
  
  #promedio de X y Y
  promx <- mean(personas)
  promy <- mean(ventas)
  
  #coeficiente de relacion
  #coe_rela <- sum(xy)/ (sum(personas)*sum(ventas))
  coe_rela <- ((sum(personas)-promx)*(sum(ventas)-promy))/sqrt(((sum(personas)-promx)**2)*((sum(ventas)-promy)**2))
  #Calculo de desviaciones estandar
  sx <- sd(personas)
  sy <- sd(ventas)
  
  #DataFrame
  da <- data.frame("Personas" = personas,
                   "Ventas" = ventas,
                   "x*y" = xy,
                   "x^2" = x2)
  #Obtencion de la formula 
  b <- (coe_rela *sy) / sx
  a = promy -(b*promx)
  
  y = a+(b*ventas)
  #Grafica la linea de regresion lineal
  graf1 + geom_smooth(mapping = aes(60.04+(4.98*ventas)))
}

#Regresion lineal 2

"Un gerente de ventas reunió los datos siguientes relacionados con las ventas 
anuales en miles de pesos y los años de experiencia de diez vendedores. 
Estime las ventas anuales para un vendedor con 7 años de experiencia"

Ejercicio2 <- function(){
  vendedor <- c(1,2,3,4,5,6,7,8,9,10)
  años_exp <- c(1,3,4,4,6,8,10,10,11,13) #x
  ventas_anuales <- c(80,97,92,102,103,111,119,123,117,136) #y
  #DataFrame
  data <- data.frame("Vendedor" = vendedor,
                     "Años de Experiencia" = años_exp,
                     "Ventas Anuales" = ventas_anuales)
  
  #Graficar lo obtenido
  f <- ggplot2::ggplot(data, aes(años_exp, ventas_anuales))
  p <- f + ggplot2::geom_point()
  
  #Producto de x*y
  i <- 1 
  xy <- integer()
  while (i <= length(años_exp)){
    xY <- años_exp[i] * ventas_anuales[i]
    xy <- c(xy, xY)
    i = i + 1
  }
  
  #X2
  x2 <- numeric()
  i <- 1 
  while (i <= length(años_exp)){
    x <- años_exp[i] * años_exp[i]
    x2 <- c(x2,x)
    i = i + 1
  }
  
  #Obtener y^2
  y2 <- numeric ()
  i <- 1 
  while (i <= length(ventas_anuales)){
    y <- ventas_anuales[i] * ventas_anuales[i]
    y2 <- c(y2,y)
    i = i + 1
  }
  
  #Promedio de x y 
  promx <- mean(años_exp)
  promy <- mean(ventas_anuales)
  
  #DEsviacion estandar
  desx <- sd(años_exp)
  desy <- sd(ventas_anuales)
  
  #se calcula el coeficiente de correlacion
  coefi_corre <- ((sum(años_exp)-promx)*(sum(ventas_anuales)-promy))/sqrt(((sum(años_exp)-promx)**2)*((sum(ventas_anuales)-promy)**2))
  
  #Calculo de b y a
  b = coefi_corre * desy / desx
  a = promy - (b*promx)
}

personas <- c(2, 6, 8, 8, 12, 16, 20, 20, 22, 26)
ventas <- c(58, 10, 88, 118, 117, 137, 157, 168, 149, 202)
mPv <- data.frame (personas,ventas)

graf = ggplot2::ggplot(mPv, aes(x=personas, y = ventas)) 
graf + geom_point()

modelo1 <- lm(ventas ~ personas, data = mPv)

summary(modelo1)

grafica + geom_point() + geom_smooth(method ="lm", color = "Red")

install.packages("psych")

pairs.panels(mPv, method = "perason")