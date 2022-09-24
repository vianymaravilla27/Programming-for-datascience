"Graficar, distribuciones T-Stdent con 10, 20, 30, 50 y 100 
grados de libertad. Analizar los resultados obtenidos."

library(KernSmooth)
library(ggplot2)

#con diez grados de libertad
diez_libertad <- rt(100,10)
ancho_barras <- dpih(diez_libertad)
nbarras <- seq(min(diez_libertad) - ancho_barras,
               max(diez_libertad) + ancho_barras, by = ancho_barras)

hist(diez_libertad, freq = F, breaks = nbarras, border = "black", main = "Histograma T-Student libertad 10")
curve(dt(x, 10), lwd = 2, col = "red", add = T)

#con veinte grados de libertad
veinte_libertad <- rt(100, 20)
ancho_barras <- dpih(veinte_libertad)
nbarras <- seq(min(veinte_libertad) - ancho_barras,
               max(veinte_libertad) + ancho_barras, by = ancho_barras)

hist(veinte_libertad, freq = F, breaks = nbarras, border = "black", main = "Histograma T-Student libertad 20")
curve(dt(x, 20), lwd = 2, col = "black", add = T)

#Con treinta grados de libertad
treinta_libertad <- rt(100, 30)
ancho_barras <- dpih(treinta_libertad)
nbarras <- seq(min(treinta_libertad) - ancho_barras,
               max(treinta_libertad) + ancho_barras, by = ancho_barras)

hist(treinta_libertad, freq = F, breaks = nbarras, border = "black", main = "Histograma T-Student libertad 30")
curve(dt(x, 30), lwd = 2, col = "blue", add = T)

#Con cincuenta grados de libertad
cincuenta_libertad <- rt(100, 50)
ancho_barras <- dpih(cincuenta_libertad)
nbarras <- seq(min(cincuenta_libertad) - ancho_barras,
               max(cincuenta_libertad) + ancho_barras, by = ancho_barras)

hist(cincuenta_libertad, freq = F, breaks = nbarras, border = "black", main = "Histograma T-Student libertad 50")
curve(dt(x, 50), lwd = 2, col = "yellow", add = T)

#con cien grados de libertad
cien_libertad <- rt(100, 100)
ancho_barras <- dpih(cien_libertad)
nbarras <- seq(min(cien_libertad) - ancho_barras,
               max(cien_libertad) + ancho_barras, by = ancho_barras)

hist(cien_libertad, freq = F, breaks = nbarras, border = "black", main = "Histograma T-Student libertad 100")
curve(dt(x, 50), lwd = 2, col = "cyan", add = T)
