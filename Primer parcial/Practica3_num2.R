"Escribe una función que dado un valor y un vector
devuelva cuántas veces se encuentra el valor en el
Una función recibe como parámetro una matriz. La
matriz representa los tiempos empleados por un
ciclista en varias etapas. Cada fila representa una
etapa. La primera columna de la matriz almacena el
número de horas, la segunda columna el número de
minutos y la tercera columna el número de
segundos que tardó en completar la etapa. Por
ejemplo, si se recibe:
  |2 | 30 | 50|
  |1 | 55 | 20|
  El ciclista ha completado dos etapas.En la primera
  etapa ha tardado 2 horas, 30 minutos, y 50 
  segundos. La funcion debe devolver una lista con el 
  número total de horas, minutos y segundos empleados 
  por el ciclista en cubrir el total de etapas. Para los datos
 de ejmplo se devolvera 4 horas, 25 minutos y 10 segundos."

Cacula_Tiempo <- function(mat,intentos){
  se <- 60
  mi <- 60
  cat("\n Dada la matriz:\n")
  print(mat)
  t_horas <- apply(mat,MARGIN = 2, FUN = sum)
  segund <- t_horas[3]
  mint <- t_horas[2]
  hor <- t_horas[1]
  if (segund > se){
    segund = segund %% 60
    mint = mint + 1
    if (mint > mi){
      mint = mint %% 60
      hor = hor + 1
      cat("El resultado es:\nHoras:",hor,"minutos:", mint,"segundos: ",segund)
    }else{
      mint = mint %% 60
      cat("El resultado es:\nHoras:",hor,"minutos:", mint,"segundos: ",segund)
    }
  }else {
    if(mint > mi){
      mint = mint %% 60
      hor = hor + 1
      cat("El resultado es:\nHoras:",hor,"minutos:", mint,"segundos: ",segund)
    }else{
      cat("El resultado es:\nHoras:",hor,"minutos:", mint,"segundos: ",segund)
    }
  }
}

Ingreso <- function (){
  horas <- integer()
  minutos <- integer()
  segundos <- integer()
  hr <- 5
  min <- 5
  sg <- 5
  n <- 0
  mat <- matrix(,nrow = 3, ncol = 3, byrow = TRUE)
  intentos <- readline("Ingresa cuantas rondas hizo el competidor: \n")
  intentos <- as.integer(intentos)
  while (n < intentos) {
    hr <- readline("\nIngresa cuantas horas tardo en terminar\n")
    hr <- as.integer(hr)
    while (hr < 0){
      if (hr < 0){
        cat("\nLa cantidad ingresada no es valido\nVuelva a intentarlo\n")
        hr <- readline("\nIngresa cuantas horas tardo en terminar\n")
        hr <- as.integer(hr)
      }
    }
    
    min <- readline("\nIngrese cuantos minutos se tardo en terminar\n")
    min <- as.integer(min)
    while (min < 0 && min < 60){
      cat("La cantidad ingresada no es valido\nVuelve a intentarlo.")
      min <- readline("\nIngrese cuantos minutos se tardo en terminar\n")
      min <- as.integer(min)
    }
    
    sg <- readline("\nIngresa cuantos segundos se tardo en terminar\n")
    sg <- as.integer(sg)
    while (sg < 0 && sg < 60){
      cat("\nLa cantidad que ingresaste no es valida\nVuelve a intentarlo.")
      sg <- readline("\nIngresa cuantos segundos se tardo en terminar\n")
      sg <- as.integer(sg)
    }

    horas <- c(horas,hr)
    minutos <- c(minutos, min)
    segundos <- c(segundos,sg)
    mat <- cbind(horas, minutos, segundos)
    n = n + 1
  }
  Cacula_Tiempo(mat, intentos)
}
