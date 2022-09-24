main <- function() {
  # Importacion de datos
  datos <- obt_datos()
  # Tabulación de los datos
  View(datos)
  # Limpieza de datos
  x <- datos[, 4:16]
  Y <- datos[, 0:3]
  View(x)
  View(Y)
  # Intervalos de confianza
  #inter_conf(x)
  # Analisis de componentes principales
  acp(datos = x)
  # Analisis de factores
  #af(x)
  # Analisis Cluster
  cluster(datos = datos)
  #ac(x)
}

obt_datos <- function() {
  # Incializamos y declaramos la ruta del proyecto
  #ruta <- "C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto3"
  #setwd(dir = ruta)
  # Importamos los datos
  datos <- read.csv(file = file.choose(), header = T)
  return(datos)
}

inter_conf <- function(datos) {
  #Cargar libreria requerida
  library(BSDA)
  
  zsum.test(mean.x = )
  
}

acp <- function(datos) {
  # Obtenemos las medias de cada columna
  media <- apply(X = datos, MARGIN = 2, FUN = mean)
  # Obtenemos las varianzas de cada columna
  varianza <- apply(X = datos, MARGIN = 2, FUN = var)
  # Obtenemos las desviaciones estandar de cada columna
  desvest <- apply(X = datos, MARGIN = 2, FUN = sd)
  # Mostramos los resultados
  desc_datos <- data.frame(media, varianza, desvest)
  print(desc_datos)
  # Generamos nuestros componentes principales de manera automatica con sd = 1.
  pca <- prcomp(datos, scale = TRUE) 
  print(pca)
  print("Media PCA:")
  print(pca$center)
  print("Desviacion tipica PCA:")
  print(pca$scale)
  print("Loadings PCA:")
  print(pca$rotation)
  print("X PCA")
  print(head(pca$x))
  # Graficamos
  biplot(x = pca, scale = 0.0, cex = c(0.5, 1), col = c("blue4", "brown3"), 
         expand = 1, xlim = c(-6.0, 2.0), ylim = c(-6.0, 2.0))
  library(ggplot2)
  # Varianzas
  print("Varianzas")
  print(pca$sdev^2)
  # Porcentaje de Varianzas
  prop_varianza <- pca$sdev^2/sum(pca$sdev^2)
  print("Porcentajes Varianzas")
  print(prop_varianza)
  # Grafica de Porcentaje de Varianzas
  plot(ggplot(data = data.frame(prop_varianza, pc = 1:8),
              aes(x = pc, y = prop_varianza)) +
         geom_col(width = 0.3) +
         scale_y_continuous(limits = c(0,1)) +
         theme_bw() +
         labs(x = "Componente principal",
              y = "Prop. de varianza explicada"))
  # Porcentaje de Varianzas Acumuladas
  prop_varianza_acum <- cumsum(prop_varianza)
  print("Porcentajes Varianzas Acumuladas")
  print(prop_varianza_acum)
  # Grafica de Porcentaje de Varianzas Acumuladas
  plot(ggplot(data = data.frame(prop_varianza_acum, pc = 1:8),
              aes(x = pc, y = prop_varianza_acum, group = 1)) +
         geom_point() +
         geom_line() +
         theme_bw() +
         labs(x = "Componente principal",
              y = "Prop. varianza explicada acumulada"))
}

cluster <- function(datos){
  library(cluster)
  datos <- read.csv(file = file.choose(), header = T)
  x <- datos[, 4:16]
  Y <- datos[, 0:3]
  #Explorar los datos
  #Desplegar la estructura de los datos
  str(x)
  
  #Obtencion de medidas estadísticas
  summary(x)
  
  #Valores na
  any(is.na(x))
  
  #Convertir los datos a un DF
  dat2 <- as.data.frame(x)
  summary(dat2)
  
  #Se aplica la medida de distancia para obtener la maatríz de distancia
  dist_mat <- dist(dat2, method = "euclidean")
  
  #Obtencion de los grupos
  grupos <- hclust(dist_mat, method = "ward.D")
  plot(grupos)
  
  #Trazar la linea de corte y mostrar rectangulo en los grupos
  lineaCorte <- cutree(grupos, k = 3)
  
  plot(grupos)
  rect.hclust(grupos, k=3, border = 2:6)
  abline(h = 3, col = "red")
}

k_means <- function(datos){
  install.packages("factoextra")
  library (tidyverse) #Manipulacion de datos
  library(cluster) #Algoritmos de clusterización
  library (factoextra) #Algoritmos de clusterización y visualizacón
  
  datos <- read.csv(file = file.choose(), header = T)
  x <- datos[, 4:16]
  Y <- datos[, 0:3]
  
  #Despliega la estructura de los datos
  str(x)
  
  #Obtencion de medidas estadísticas
  summary(x)
  
  #Valores na
  any(is.na(x))
  
  #se aplica la medida de distacia para obtener la matriz de distancia
  dist_mat <- dist(x, method = "euclidean")
  
  #se aplica el algoritmo k-means
  gruposk2 <- kmeans(dist_mat, centers = 3, nstart = 25)
  
  #Graficar los grupos
  fviz_cluster(gruposk2, data = dist_mat)
}

codo <- function(data){
  set.seed(1234)
  wcss <- vector()
  for(i in 1:20){
    wcss[i] <- sum(kmeans(data, i)$withinss)
  }
  ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
    geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
    ggtitle("MÃ©todo del Codo") + 
    xlab('Cantidad de Centroides k') + 
    ylab('WCSS')
  
  set.seed(1234)
  kmeans <- kmeans(data, 8, iter.max = 1000, nstart = 10)
  
  data$cluster <- kmeans$cluster
  ggplot() + geom_point(aes(x = Age, y = Weight , color = cluster), data = data, size = 2) +
    scale_colour_gradientn(colours=rainbow(4)) +
    geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
    ggtitle('Clusters de Datos con k = 8 / K-Medios') + 
    xlab('X') + ylab('Y')
}
main()
