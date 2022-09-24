" ISBN titulo nom_autor apellido_autor precio
567 El alquimista Paulo Coehlo Coehloe 145.0
123 Aleph Paulo Coehlo 245.5
378 El peregrino Paulo Coehlo 450.89

Con base en la tabla anterior cree un dataframe 
Imprima la estructura del dataframe
Ordénelo de acuerdo al isbn del libro 
Calcule el promedio del precio
Seleccione solo el título de los libros existentes"

isbn <- c(567,123,378)
titulo <- c("El alquimista","Aleph","El peregrino")
nom_autor<-c("Paulo", "Paulo","Paulo")
apellido_autor <- c("Coehlo","Coehlo","Coehlo")
precio <- c(145.0,245.5,450.89)

datosLibros <- data.frame(isbn,titulo,nom_autor,apellido_autor,precio)

#Imprima la estructura del dataframe

str(datosLibros)

#ordenelo de acuerdo al isb del libro

datosLibros[order(isbn)]

#clauclule el promedio del precio

promedio <- mean(datosLibros$precio)

#Seleccione solo el titulo de los libros existentes

datosLibros$titulo

datosLibros[, "titulo"]

datosLibros[, 2]

attach(datosLibros)
titulo
