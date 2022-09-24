#Problema de examen.
#Maravilla Pérez Vianey

#Declaracion de librerias necesarias
library(nortest)
library(factoextra)
library(ggplot2)
library(polycor)
library(ggcorrplot)
library(psych)


main <- function(){
  datos <- abrir_archivo()
  datos <- data.frame(datos)
  #View(datos)
  datos_txt <- names(datos)
  #Prueba de hipotesos
  prueba_hipotesis(datos = x$laufkont)
  
  #Escalamos los datos
  datos_esc <- scale(datos)
  
  #Análisis factorial
  af(datos = datos)
  
  #Análisis de k-means
  rel_km <- k_means(datos = datos_esc)
}

abrir_archivo <- function(){
  x <- read.table(file.choose(), header=TRUE)
  return(x)
}

prueba_hipotesis <- function(datos){
  cat("Exploracion de datos para ver si tiene una distribucion normal\n")
  print("Anderson-Darling normality test\n")
  print(ad.test(datos))
  cat("\nComo el p-value es menor a 0.5 podemos descartar que tenga una distribucion normal")
  print(t.test(datos, alternative = "less", conf.level = 0.95))
  
}

af <- function(datos) {
  
  # Obtenemos la matriz de correlacion policorica
  mat_cor <- hetcor(datos)$correlations #matriz de correlacion policorica
  plot(ggcorrplot(mat_cor, type="lower", hc.order = T, title = "Grafica de correlaciones"))
  
  # Verificamos que la matriz sea factoriazble
  cortest.bartlett(mat_cor, n = 100)->p_esf
  cat("\nBartlett Test\n")
  print(p_esf$p)
  cat("\nKMO\n")
  print(KMO(mat_cor))
  
  # Determinar el numero de factores
  plot(scree(mat_cor))
  plot(fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres"))
  
  #Rotacion
  rot<-c("varimax")
  
  bi_mod<-function(tipo){
    biplot.psych(fa(datos, nfactors = 4, fm = "minres", rotate = tipo),main = paste("Biplot con rotación",tipo),col=c(2,3,4),pch = c(21,18))  
  }
  sapply(rot,bi_mod)
  
  # Interpretacion
  modelo_varimax<-fa(mat_cor,nfactors = 4,rotate = "varimax",
                     fa="minres")
  fa.diagram(modelo_varimax, main = "Gráfico AF")
  print(modelo_varimax$loadings,cut=0) 
}

k_means <- function(datos){
  
  #se aplica el algoritmo k-means
  grupos <- kmeans(datos, centers = 4, nstart = 25)
  
  #Graficar los grupos
  plot(fviz_cluster(grupos, data = datos, main = "Clustering K medias"))
  
  relaciones <- data.frame(grupos_km = grupos$cluster)
  
  return (relaciones)
}



main()