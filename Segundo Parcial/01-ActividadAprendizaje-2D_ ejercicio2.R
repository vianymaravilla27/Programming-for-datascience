"Del portal de datos abiertos de la Cd. De México (https://datos.cdmx.gob.mx/dataset/)
descarga un archivo con información de tu interés en formato xls; además, agrega una fila
adicional y nombre a las columnas; así como, crea un archivo llamado datos_Update.csv. 
ruta entorno
getwd()"
#Se importa la libreria necesaria para operar archivos xls 
library(readxl)
library(tidyverse)

main <- function(){
  #funcion principal que llama a las demás funciones 
  ruta <- buscarRuta ()
  data_cdmx <- abrirArchivo (ruta)
  data_cdmx <- agregarFila(data_cdmx)
  rutaPrincipal <- getwd()
  guardarArchivo (rutaPrincipal, data_cdmx)
}

buscarRuta <- function (){
  #Toma la direccion que retorna file.choose() y la combierte a tipo caracter para que r pueda interpretarla
  xl <- file.choose()
}

abrirArchivo <- function (ruta){
  #Se abre el archivo y al mismo tiempo se le cambia el nombre a las columnas 
  data_excel <- read_excel(ruta, col_names = c("Columna1","Columna 2", "Columna 3") )
}
agregarFila <- function (data_cdmx){
  #Se agrega una fila al final del archivo que ya se abrio previamente y se visualiza el cambio
  vec <- list("Vianey Maravilla", "Practica del dia", 20210321)
  data_cdmx <- rbind(data_cdmx, vec)
  View(data_cdmx)
}

guardarArchivo <- function (rutaPrincipal, data_cdmx){
  #Se guarda el archivo ya modificado con el nuevo nombre en la ruta del direcctorio de trabajo
  rutaPrincipal <- str_c(rutaPrincipal, "/datos_Update.csv")
  write.csv(data_cdmx, file = rutaPrincipal)
  cat("Archivo guardado de forma correcta")
}
main ()