"
Grafica una distribuvion normal con media de 10
y desviacion estandar de 2
"

library (modeest)
library (ggplot2)


hist(rnorm(n = 100, mean = 10, sd = 2), freq = F, border = "gray50", main = "Histograma con n = 100")
curve(dnorm(x, mean = 10, sd = 2), lwd = 2, col = "blue", add = T)
