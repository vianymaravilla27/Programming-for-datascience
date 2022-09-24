"1.	Declare una estructura de datos  (dataframe) Alumno que considere, los siguientes datos:
  •	matrícula
•	nombre,
•	apellidoP,
•	apellidoM,
•	carrera,
•	calf1, calf2, calf3, calf4, calf5;
Considere la información de  5 alumnos, dichos datos deberán de ser escritos a un archivo separado por comas"

main <- function (){
  #Funcion que solo contiene la cantiddad de alumnos que se van a registrar almacenada en una variable y llama a la funcion donde se realiza el pedido de datos y posterior almacenado en el dataframe Akumno
  n <- 5
  Llenado (n)
}

Llenado <- function (n){
  i <- 1
  matricula <- character()
  nombre <- character ()
  apellidoP <- character ()
  apellidoM <- character ()
  carrera <- character ()
  calif1 <- numeric ()
  calif2 <- numeric ()
  calif3 <- numeric ()
  calif4 <- numeric ()
  calif5 <- numeric ()
  Alumno <- data.frame(matricula, nombre, apellidoP, apellidoM, carrera, calif1, calif2, calif3, calif4, calif5)
  while (i <= n){
    mat = readline ("Ingrese su matricula: \n")
    matricula <- c(matricula,mat)
    nom = readline("Ingrese su nombre:\n")
    nombre <- c (nombre, nom)
    apelP =readline ("Ingrese su apellido paterno: \n")
    apellidoP <-  c(apellidoP, apelP)
    apelM = readline ("Ingrese su apellido materno: \n")
    apellidoM <- c(apellidoM, apelM)
    car = readline ("Ingrese su carrera: \n")
    carrera <- c(carrera, car)
    cal1 = readline ("Ingrese la calificacion de la primer materia:\n")
    cal1 <- as.numeric(cal1)
    while(cal1 < 0 || cal1 > 10){
      cat("Valor no valido vuelva a intentarlo\n")
      cal1 = readline ("ingrese la calificacion de la primer materia:\n")
      cal1 <- as.numeric(cal1)
    }
    calif1 <- c(calif1, cal1)
    cal2 = readline ("ingrese la calificacion de la segunda materia:\n")
    cal2 <- as.numeric(cal2)
    while(cal2 < 0 || cal2 > 10){
      cat("Valor no valido vuelva a intentarlo\n")
      cal2 = readline ("Ingrese la calificacion de la primer materia:\n")
      cal2 <- as.numeric(cal2)
    }
    calif2 <- c(calif2, cal2)
    cal3 = readline ("Ingrese la calificacion de la tercera materia:\n")
    cal3 <- as.numeric(cal3)
    while(cal3 < 0 || cal3 > 10){
      cat("Valor no valido vuelva a intentarlo\n")
      cal3 = readline ("Ingrese la calificacion de la primer materia:\n")
      cal3 <- as.numeric(cal3)
    }
    calif3 <- c(calif3, cal3)
    cal4 = readline ("Ingrese la calificacion de la cuarta materia:\n")
    cal4 <- as.numeric(cal4)
    while(cal4 < 0 || cal4 > 10){
      cat("Valor no valido vuelva a intentarlo\n")
      cal4 = readline ("Ingrese la calificacion de la primer materia:\n")
      cal4 <- as.numeric(cal4)
    }
    calif4 <- c(calif4, cal4)
    cal5 = readline ("Ingrese la calificacion de la quinta materia:\n")
    cal5 <- as.numeric(cal5)
    while(cal5 < 0 || cal5 > 10){
      cat("Valor no valido vuelva a intentarlo\n")
      cal5 = readline ("Ingrese la calificacion de la primer materia:\n")
      cal5 <- as.numeric(cal5)
    }
    calif5 <- c(calif5, cal5)
    
    Alumno <- cbind(matricula, nombre, apellidoM,apellidoP, carrera,calif1, calif2, calif3, calif4, calif5)
    i = i + 1
  }
  write.csv(Alumno, file ="Alumno.csv", sep = ",")
  Alumno
}

main()