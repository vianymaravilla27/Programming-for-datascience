#Ptactica 10 del tercer parcial 


#Declaracion de todas las librerias a utilizar
library (tidyverse) #Manipulacion de datos
library(cluster) #Algoritmos de clusterización
library (factoextra) #Algoritmos de clusterización y visualizacón
library (readxl) #Abrir archivos de excel
library (gridExtra)
library(ggproto)

#Declaracion de funciones a emplear e inicio del codigo
main <- function(){
  #Cargamos el archivo xls
  data <- Obtenerxls()
  
  #Estructura de los datos
  str(data)
  
  #obtenemos las medidas estadisticas de los datos
  summary(data)
  
  #Aplicamos el algoritmo de kluster
  clusterJ(data)
  
  #Aplicamos el metodo k-means
  k-mean(data)
  
  #Aplicamos el algoritmo del codo para obtener el número de grupos adecuados para la muestra
  codo(data)
  
  Grupo(data)
}

Obtenerxls <- function (){
  data <- read_xls(file.choose())
  View(data)
  summary(data)
  str(data)
  return(data)
}

clusterJ <- function(data){
  #Obtenemos la matriz de distancia
  dist_mat <- dist(data, method = 'manhattan')
  
  #Obtencion de los grupos
  grupos <- hclust(dist_mat, method = "complete")
  plot(grupos)
  
  #Trazar linea de corte y mostrar rectangulo en los grupos 
  lineaCorte <- cutree(grupos, k = 3)
  plot(grupos)
  rect.hclust(grupos, k= 3, border = 2.6)
  abline(h=3, col='red')
}

k-mean <- function(data){
  #Se aplica la medida de distancia para obtener la matriz de distancia
  dis_m <- dist(data, method = "manhattan")
  
  #se aplica el algoritmo k-means
  gruposk2 <- kmeans(dis_m, centers= 3, nstart = 25)
  fviz_cluster(gruposk2, data = data)
  
}

codo <- function(data){
  set.seed(1234)
  wcss <- vector()
  for(i in 1:20){
    wcss[i] <- sum(kmeans(data, i)$withinss)
  }
  ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
    geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
    ggtitle("Método del Codo") + 
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

Grupo <- funtion(data){
  set.seed(1234)
  km_clusters <- kmeans(x = data, centers = 6, nstart = 50)
  
  # Las funciones del paquete factoextra emplean el nombre de las filas del
  # dataframe que contiene los datos como identificador de las observaciones.
  # Esto permite añadir labels a los gráficos.
  fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE,
               ellipse.type = "manhattan", star.plot = TRUE, repel = TRUE) +
    labs(title = "Resultados clustering K-means") +
    theme_bw() +
    theme(legend.position = "none")
}

main()
