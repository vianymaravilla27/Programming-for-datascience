#Ejemplo de case basado en rpubs

library(openxlsx)
library(corrplot)
library(corrr)
library(psych)
library(stats)

acestudiantes <- read.xlsx(xlsxFile= file.choose())
estudiantes

matriz_correlaciones <- cor(estudiantes, use = "pairwise.complete.obs")
matriz_correlaciones

corrplot(cor(estudiantes), order = "hclust", tl.col = "black", tl.cex = 1)

estudiantes_correlaciones <- correlate(estudiantes)
rplot(estudiantes_correlaciones, legend = TRUE, colours = c("firebrick1", "black",
                                                             "darkcyan"),print_cor = TRUE)
det(matriz_correlaciones)

bartlett.test(estudiantes)
KMO(estudiantes)

factanal(estudiantes, factors = 2, rotation = "none")

puntuaciones <- factanal(estudiantes, factors = 2, rotation = "none", scores = "regression")$scores
estudiantes <- cbind(estudiantes, puntuaciones)
estudiantes$Factor1 <- round(((estudiantes$Factor1 - min(estudiantes$Factor1))/(max(estudiantes$Factor1) - 
                                                                                  min(estudiantes$Factor1))), 2)
estudiantes

hist(estudiantes$Factor1, freq = TRUE, main = "Gráfico de la Distribución del Factor 1", 
     xlab = "Factor 1", ylab = "Frecuencia", col = "#009ACD")
