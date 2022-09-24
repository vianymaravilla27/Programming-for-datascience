"Cree una lista que contenga nombre y apellidos de 3 alumnos y 
sus calificaciones


Nombre las etiquetas de la lista
Agregue las edades de los alumnos
Elimine la lista de las edades de los alumnos
"
listacal <- list(matrix(c("Erin","Mondolla",
                          "Antonio", "Vazquez",
                          "Jacob", "Salinas"),nrow = 3, ncol = 2, byrow =T),
                 c(9,6,5))
#Nombre las etiquetas de la lista 

names(listacal) <- c("nNombreAp","calif")

'Agregue las edades de los alumnos'

edades <- list(c(23,19,25))
listacal <- append(listacal,edades)

#listacal <- c(listcal, edades)

#Eliminar la lista de las edades de los alumnos

listacal[[3]]<- NULL

#Seleccionar un apellido

listacal[["nNombreAp"]][,2]

listacal [["calif"]][3]

