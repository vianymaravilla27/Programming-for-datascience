#Practica 9 
#Ejercicio número 2

library(ggplot2)
library(psych)
library(polycor)
library(ggcorrplot)
library(GPArotation)


bfi_s <- read.csv(file.choose()) # subconjunto de datos
#bfi_s <- data.frame(bfi_s$pruebas_totales, bfi_s$positivos_totales, bfi_s$pruebas_totales_cdmx,
 #          bfi_s$positivos_totales_cdmx, bfi_s$tasa_positividad, bfi_s$tasa_positividad_cdmx )
X <- bfi_s[, 4:16]
Y <- bfi_s[, 0:3]
bfi_s <- X
mat_cor <- hetcor(bfi_s)$correlations #matriz de correlación policorica
ggcorrplot(mat_cor,type="lower",hc.order = T)

#Paso 1.1 Verificar que la matriz sea factorizable.
cortest.bartlett(mat_cor) -> p_esf
p_esf$p
KMO(mat_cor)

#Paso 2: Escoger un método para extraer los factores

### prueba de dos modelos con tres factores
modelo1<-fa(mat_cor,
            nfactors = 3,
            rotate = "none",
            fm="mle") # modelo máxima verosimilitud

modelo2<-fa(mat_cor,
            nfactors = 3,
            rotate = "none",
            fm="minres") # modelo minimo residuo
######comparando las comunalidades
sort(modelo1$communality,decreasing = T)->c1
sort(modelo2$communality,decreasing = T)->c2
head(cbind(c1,c2))

####comparacion de las unicidades 
sort(modelo1$uniquenesses,decreasing = T)->u1
sort(modelo2$uniquenesses,decreasing = T)->u2
head(cbind(u1,u2))

#Paso 3: Determinar el número de factores
scree(mat_cor)
fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres")

#Paso 4: Rotar la matriz
#Rotaciones
rot<-c("none", "varimax", "quartimax","Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(bfi_s,nfactors = 2,fm="minres",rotate = tipo),main = paste("Biplot con rotación ",tipo),col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}
sapply(rot,bi_mod)

#Paso 5: La interpretación
modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax",
                   fa="minres")
fa.diagram(modelo_varimax)
print(modelo_varimax$loadings, cut = 0)
print(fviz_cluster(grupos, data = datos, main = "Clustering K medias"))
print(fviz_cluster(list(data = mat_dist, cluster = lineaCorte), main = "Clustering Jer�rquico"))
