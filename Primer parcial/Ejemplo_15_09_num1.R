"Crea una funcion que tome los nombres de los archivos
archivos <- c('ventas.csv_20160522_zacatecas', pedidos_20160422_sinaloa')
Genere unatabla con una fila porfichero y tres columnas: el nombre del fichero, la fecha y el estado"

principal <- function(){
  archivos <- c("ventas.csv_20160522_zacatecas", "pedidos_20160422_sinaloa")
  cat("Los datos de entrada son: \n",archivos)
  descomp(archivos)
}

descomp <- function (archivos){
  al <- str_split(archivos, "_")
  cat("La descomposicion es:")
  al
  LlenaDatos(al)
}

LlenaDatos <- function (al){
  daArchivos <- data.frame(matrix(vector(),0,3))
  
  for (i in 1:length(al)){
    vector <- c(al[[i]][1], al[[i]][2], al[[i]][3])
    daArchivos <- rbind(daArchivos, vector)
  }
  
  names(daArchivos) <- c("Nombre", "Fecha", "Lugar")
  cat("los datos de la tabla son:\n")
  daArchivos
  
}

principal()
"
archivos <- "ventas 20160522 zacatecas pedidos 20160422 sinaloa"

read.table(text=archivos,col.names = c("NOMBRE", "FECHA", "LUGAR"))



r <- unlist(strsplit(archivos, " "))
data.frame(nombre=r[c(FALSE,TRUE)], fecha=r[c(FALSE,TRUE)],lugar=r[c(FALSE,TRUE)])"