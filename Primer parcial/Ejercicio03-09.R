"
Cree un arreglo en R los siguientes datos

1 4 
2 5 
3 6 

7 10
8 11
9  12
Cree el arreglo 
Agregue etiquetas al arreglo  
Obtenga las dimensiones del arreglo
Obtenga el elemento del grupo de 2 de la segunda fila
"
array.ejemplo <- array(seq(1:12),c(3,2,2))

dimnames(array.ejemplo)<- list(c("e1","e2","e3"),
                               c("Col1","col2"),
                               c("Gp1","Gp2"))

#Obtenfa el elemento del grupo de 2 de la segunda fila

array.ejemplo[2, , 2]

array.ejemplo["e2", , "Gp2"]

