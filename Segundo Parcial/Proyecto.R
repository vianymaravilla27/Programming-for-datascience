library(tidyverse)
library (modeest)
library (ggplot2)
library(ggcorrplot)
library(nortest)

main <- function(){
  dat <- abrirArchivo()
  dat <- dat[dat$Global_Sales <= 5 & dat$Global_Sales > 0,]
  
  medidas_na <- estimadores(columna = dat$NA_Sales)
  medmedidas_eu <- estimadores(columna = dat$EU_Sales)
  medidas_jp <- estimadores(columna = dat$JP_Sales)
  medidas_oth <- estimadores(columna = dat$Other_Sales)
  medidas_gen <- estimadores(columna = dat$Global_Sales)
  
  imprimir_estimadores(nombre = "Ventas NA", estimadores = medidas_na)
  imprimir_estimadores(nombre = "Ventas Eu", estimadores = medmedidas_eu)
  imprimir_estimadores(nombre = "Ventas JP", estimadores = medidas_jp)
  imprimir_estimadores(nombre = "Otras ventas", estimadores = medidas_oth)
  imprimir_estimadores(nombre = "Ventas generales", estimadores = medidas_gen)
  
  datos <- data.frame(Year = as.numeric(dat$Year),
                      NA_Sales = as.numeric(dat$NA_Sales),
                      EU_Sales = as.numeric(dat$EU_Sales),
                      JP_Sales = as.numeric(dat$JP_Sales), 
                      Other_Sales = as.numeric(dat$Other_Sales), 
                      Global_Sales = as.numeric(dat$Global_Sales))
  obtener_correlacion(datos)
  verificar_normalidad(variable = dat$Global_Sales)
  verificar_normalidad(variable = dat$NA_Sales)
  aplicar_regresion_lineal(y = dat$Global_Sales, x = dat$NA_Sales)
}

abrirArchivo <- function (){
  f <- read.csv(file.choose())
  View (f)
  return (f)
}

estimadores <- function(columna){
  ordenado <- sort(columna)
  
  media <- mean(ordenado)
  mediana <- median(ordenado)
  moda <- mfv(ordenado)
  cuartiles <- quantile(x = ordenado, probs = c(0.25, 0.5, 0.75))
  deciles <- quantile(x = ordenado, probs = seq(0.1, 0.9, 0.1))
  rango <- (ordenado[which.max(ordenado)]) - (ordenado[which.min(ordenado)])
  varianza <- var(ordenado)
  desviacion <- sd(ordenado)
  coeficiente <- varianza / media
  
  #estimadores <- c(media, mediana, moda, cuartiles, deciles, rango, varianza, desviacion, coeficiente)
  estimadores <- list(media, mediana, moda, cuartiles, deciles, rango, varianza, desviacion, coeficiente)
  
  return(estimadores)
}

imprimir_estimadores <- function(nombre, estimadores) {
  cat("\nNombre de la variable: ", nombre, "\n")
  cat("Media: ", estimadores[[1]], "\n")
  cat("Mediana: ", estimadores[[2]], "\n")
  cat("Moda(s): ", estimadores[[3]], "\n")
  cat("Cuartiles: ", estimadores[[4]], "\n")
  cat("Deciles: ", estimadores[[5]], "\n")
  cat("Rango: ", estimadores[[6]], "\n")
  cat("Varianza: ", estimadores[[7]], "\n")
  cat("Desviacion estándar: ", estimadores[[8]], "\n")
  cat("Coeficiente de variacion: ", estimadores[[9]], "\n")
}

aplicar_regresion_lineal <- function(x, y){
  modelo <- lm(y~x)
  
  
  a = modelo[[1]][1]
  b = modelo[[1]][2]
  cat("\nEcuacion: y =", a, "+", b, "x\n")
  
  #Funcion anonima para la recta
  func_recta <- function(x) a + b * x
  
  x_izq <- x[which.min(x)]
  x_der <- x[which.max(x)]
  
  y_izq <- func_recta(x_izq)
  y_der <- func_recta(x_der)
  y_inf <- 0
  y_sup <- 0
  if (y_izq >= y_der) {
    y_inf <- y_der
    y_sup <- y_izq
  }
  else {
    y_inf <- y_izq
    y_sup <- y_der
  }
  
  # Graficacion de la recta
  recta_reg = plot(func_recta, col = "black", lwd = 4,  xlim = c(x_izq, x_der),  ylim = c(y_inf, y_sup), xlab = "", ylab = "")
  recta_reg
  par(new = TRUE)
  
  # Graficacion del diagrama de dispersion previo a la recta
  plot(x, y, pch = 19, col = "blue",  xlim = c(x_izq, x_der), ylim = c(y_inf, y_sup), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
}

verificar_normalidad <- function(variable) {
  # Mediante la grÃ¡fica
  hist(variable, freq = F, border = "gray50")
  lines(density(variable), lwd = 2)
  curve(dnorm(x, mean(variable), sd(variable)), lwd = 2, col = "blue", add = T)
  qqnorm(variable, pch=20, col="blue")
  qqline(variable, col="red", lwd=2)
  
  # Mediante la funciÃ³n
  norm_lillie <- lillie.test(variable)
  print(norm_lillie)
}

obtener_correlacion <- function (datos) {
  
  metodo <- "pearson"
  
  # Matriz
  matriz_Corr <- round(cor(datos, method = metodo), 2)
  print(matriz_Corr)
  grafica <- ggcorrplot(matriz_Corr, method = "circle")
  print(grafica)
}

main()