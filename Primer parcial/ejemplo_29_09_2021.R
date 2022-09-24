"Descargar el archivo de vinos del repositorio UCI ML
https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data

Importar los datos descargados
Visualizar los datos exportados
Agregar una fila de datos
Guardar los datos en un archivo CSV con el nombre wineModif.data"

main <- function(){
  #descargadearchivo ()
  datos = importarDatos()
  datos = agregaDatos(datos)
  exportarDatos (datos)
}

descargadearchivo <- function (){
  download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                destfile = "D:\\viane\\Descargas\\wine.data")
}

importarDatos <- function () {
  #datos = scan(file = "D:\\viane\\Descargas\\wine.data", sep = ",")
  #datos = read.table("D:\\viane\\Descargas\\wine.data", sep = ",")
  datos <- read.csv(file = "D:\\viane\\Descargas\\wine.data", header = FALSE)
  
  View(datos)
}

agregaDatos <- function (datos) {
  v1 <- c(5,14.13,
          4.10,
          2.74,
          24.5,
          96,
          2.05,
          0.76,
          0.56,
          1.35,
          9.200000,
          0.610,
          1.60,
          560)
  datos <- rbind(datos, v1)
  View(datos)
}

exportarDatos <- function (datos){
  write.table(datos,"D:\\viane\\Descargas\\wineModif.data", sep =",", row.names = FALSE, col.names = FALSE)
  
  cat("Datos exportados")
}
main()