---
title: "Examen"
author: "Vianey Maravilla Pérez"
date: "14/12/2021"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup}
#knitr::opts_chunk$set(echo = TRUE)
library(nortest)
library(factoextra)
library(ggplot2)
library(polycor)
library(ggcorrplot)
library(psych)
datos <- read.table(file.choose(), header=TRUE)
datos <- data.frame(datos)
datos_esc <- scale(datos)
```

## Resolucion de examen.

Primero cargamos los datos al entorno de trabajo y lo transformamos a un data frame para poder trabajar más comodo don dichos datos, empezaremos con la prueba de hipotesis, ya que en esta podemos manipular los datos tal cual esta, en las demás pruebas es necesario escalar los datos, por eso empezaremos con esta, aplicaremos una prueba de normalidad.

```{r cars}
  cat("Exploracion de datos para ver si tiene una distribucion normal\n")
  print("Anderson-Darling normality test\n")
  print(ad.test(datos$laufkont))
  cat("\nComo el p-value es menor a 0.5 podemos descartar que tenga una distribucion normal")
  print(t.test(datos$laufkont, alternative = "less", conf.level = 0.95))
```

## Analisis factorial.

Como se habia mencionado anteriormente para trabajar esta técnica tenemos que tener los datos escalados, por lo se realiza eso y procedemos a realizar el análisis, utilizamos una funcion la que nos muestra cual es el número de factores optimos para trabajar con nuestro data set, el resultado son los siguientes:

```{r pressure2, echo=FALSE}
# Obtenemos la matriz de correlacion policorica
  mat_cor <- hetcor(datos_esc)$correlations #matriz de correlacion policorica
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
    biplot.psych(fa(datos_esc, nfactors = 4, fm = "minres", rotate = tipo),main = paste("Biplot con rotación",tipo),col=c(2,3,4),pch = c(21,18))  
  }
  sapply(rot,bi_mod)
  
  # Interpretacion
  modelo_varimax<-fa(mat_cor,nfactors = 4,rotate = "varimax",
                     fa="minres")
  fa.diagram(modelo_varimax, main = "Gráfico AF")
  print(modelo_varimax$loadings,cut=0) 
```

Obtenemos la matriz de correlaciones, asi como el grafico donde muestra el número de factores requeridos y la relación que tiene las variables con cada factor.

## k-means

Para trabajar con el k-means nos guiamos un poco con el analisis anterior, definimos 3 grupos para trabajar con este análisis y obtenemos el siguiente resultado.


```{r pressure1, echo=FALSE}
#se aplica el algoritmo k-means
  grupos <- kmeans(datos_esc, centers = 3, nstart = 25)
  
  #Graficar los grupos
  plot(fviz_cluster(grupos, data = datos_esc, main = "Clustering K medias"))
  
  relaciones <- data.frame(grupos_km = grupos$cluster)

```

