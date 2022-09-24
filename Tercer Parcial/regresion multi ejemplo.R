#Regrecion lineal múltiple
 
#Leer datos
datos <- read.cvs("https://raw.githubusercontent.com/fhernanb/Python-para-estadistica/master/03%20Regression/Regresi%C3%B3n%20lineal%20simple/softdrink.csv", header = T)

#Observacion la base de datos
names(datos) # Ver nombre de las variables
str(datos)   # Estructura de la base
datos1 <- datos[,-1] # Elimina la primera columna
names(datos1)

#coeficiente de correlación
cor(datos1$x1, datos1$x2)

#Matriz de correlación
round(cor(datos1), 3) # Matriz de correlación redondeada a 3 decimales


#Matriz de dispersion
plot(datos1)
# Opción mejorada 
pairs(datos1, labels=c("y","x1","x2"), main='Matriz de dispersión', cex.main=0.8, cex = 1.5, pch = 20, bg="light blue", cex.labels = 1, font.labels=1)

#Matriz de dispersión con correlacions 
panel.cor <- function(x1, y, digits=2, prefix="", cex.cor){
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x1, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(datos1, lower.panel=panel.smooth, upper.panel=panel.cor, 
      main="Matriz de dispersión con correlaciones")

#Ajustar el modelo ajustado
mod <- lm(y ~ x1 + x2, data = datos1)

#Resulrados del modelo ajustado
summary (mod)


#Otras formas de obtener los mismo valores

# Coeficientes
coef(mod)               # Opción 1

## (Intercept)          x1          x2 
##  2.34123115  1.61590721  0.01438483

mod$coef                # Opción 2
## (Intercept)          x1          x2 
##  2.34123115  1.61590721  0.01438483
# Varianza estimada
sigma(mod)^2            # Opción 1 
summary(mod)$sigma^2    # Opción 2


# Desviación estándar 
sigma(mod)              # Opción 1

summary(mod)$sigma      # Opción 2

#Intervalos de confianza para los coeficientes del model
cofint(mod, conf.level=0.95) #I.C al 95%

#Gráfico de dispersión 3D
# install.packages("plot3D")
# install.packages("plot3Drgl")
library(plot3D)
library(plot3Drgl)

grid.lines <- 40
x1.pred <- seq(min(datos1$x1), max(datos1$x1), length.out = grid.lines)
x2.pred <- seq(min(datos1$x2), max(datos1$x2), length.out = grid.lines)
x1x2 <- expand.grid(x1 = x1.pred, x2 = x2.pred)
y.pred <- matrix(predict(mod, newdata = x1x2), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(mod)
scatter3D(x=datos1$x1, y=datos1$x2, z=datos1$y, theta=30, phi=8, pch=20)

#Gráfico de disperción con plano de regresión
grid.lines <- 40
x1.pred <- seq(min(datos1$x1), max(datos1$x1), length.out = grid.lines)
x2.pred <- seq(min(datos1$x2), max(datos1$x2), length.out = grid.lines)
x1x2 <- expand.grid(x1 = x1.pred, x2 = x2.pred)
y.pred <- matrix(predict(mod, newdata = x1x2), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(mod)

scatter3D(x=datos1$x1, y=datos1$x2, z=datos1$y, theta=30, phi=8, pch=20, bty = "g", 
          colkey = TRUE, surf = list(x=x1.pred, y=x2.pred, z=y.pred, fit = fitpoints))
