'''Conversion de datos a diferentes sistemas de medicion'''
cat('Elija el sistema al que quiera convertirlo:\n')
cat("1.-ingles\n2.-internacional\n")

opc <- readLines(n=1)
if(opc =='1' || opc ==' Ingles' || opc =='ingles'){
  cat('ingrese la cantidad de centimetros a transformar')
  cantimetros<- readLines(n=1)
  cantimetros<- as.numeric(cantimetros)
  cat('Seleccione en que medicion quiere verlo:\n1.-pie\n2.-yardas\n3.-Pulgadas\nIngrese el numero de la opcion que quiere:')
  opcion_medida<-readLines()
  if(opcion_medida == '1'){
    pie<-cantimetros *(1/30.47)
    cat('El valor en pies es de:', pie)
  }
  if(opcion_medida == '2'){
    yarda<-cantimetros *(1/91.44)
    cat('El valor en yarda es de:', pie)
  }
  if(opcion_medida == '3'){
    pulgadas<-cantimetros *(1/2.54)
    cat('El valor en yarda es de:', pie)
  }
}
if(opc =='2' || opc ==' Internacional' || opc =='internacional'){
  cat('Seleccione en que medicion tiene su medida:\n1.-pie\n2.-yardas\n3.-Pulgadas\nIngrese el numero de la opcion que quiere:')
  opcion_medida<-readLines()
  if(opcion_medida == '1'){
    cat('ingrese la cantidad de pies a transformar')
    pie<- readLines(n=1)
    pie<- as.numeric(cantimetros)
    centi<-pie *(1/0.0328084)
    cat('El valor de pies en centimetros es:', pie)
  }
  if(opcion_medida == '2'){
    cat('ingrese la cantidad de yardas a transformar')
    yardas<- readLines(n=1)
    yardas<- as.numeric(cantimetros)
    centi<- yardas *(1/0.010936133333333)
    cat('El valor de yardas en centimetros es:', pie)
  }
  if(opcion_medida == '3'){
    cat('ingrese la cantidad de pulgadas a transformar')
    pulgadas<- readLines(n=1)
    pulgadas<- as.numeric(cantimetros)
    centi<- yardas *(1/0.393700799999988027)
    cat('El valor de pulgadas en centimetros es:', pie)
  }

}