"
Graficar una distribución F con v1 = 10 y v2 = 20 grados de libertad.
Analizar los cambios que esta distribución tiene al invertir los grados 
de libertad del numerador y del denominador.
"
library(KernSmooth)

da <- rf(100,10, 20)

ancho_barras <- dpih(da)

nbarras <- seq(min(da) - ancho_barras,
               max(da) + ancho_barras, by = ancho_barras)

hist(da, freq = F, breaks = nbarras, border = "gray50", main = "Histograma con n = 100")
curve(df(x,10, 20), lwd = 2, col = "blue", add = T)

da <- rf(100,20, 10)

ancho_barras <- dpih(da)

nbarras <- seq(min(da) - ancho_barras,
               max(da) + ancho_barras, by = ancho_barras)
hist(da, freq = F, breaks = nbarras,border = "gray50", main = "Histograma con n = 100")
curve(df(x,20, 10), lwd = 2, col = "red", add = T)
