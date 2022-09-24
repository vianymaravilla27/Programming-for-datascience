#Practica de analisis de componentes principales ejercicio número 2
#Se carga el data set correspondinete 
vino <- read.csv(file.choose())
vino <- data.frame(vino$fixed.acidity,vino$volatile.acidity, 
                   vino$citric.acid,vino$residual.sugar, 
                   vino$chlorides,vino$free.sulfur.dioxide, 
                   vino$total.sulfur.dioxide, vino$density, 
                   vino$pH, vino$sulphates,
                   vino$alcohol, vino$quality)
#View(vino)
#Descripcion de los datos
media <- apply(vino, MARGIN = 2, FUN = mean)
varianza <- apply(vino, MARGIN = 2, FUN = var)
desviacion <- apply(vino, MARGIN = 2, FUN = sd)

descr_file <- data.frame(media, varianza, desviacion)
descr_file

#Se estandarizan las variables 
acp <- prcomp(vino, scale = TRUE)
names(acp)
acp$center #media
acp$scale #Sd
acp$rotation #componentes principales

biplot(x = acp, scale = 0.0, cex = 0.5, col = c("blue4", "red"))
