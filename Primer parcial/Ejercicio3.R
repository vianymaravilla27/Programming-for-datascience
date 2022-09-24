'''De una lista de presidentes de Mexico y una lista de mandatos imprima
los siguientes: El presidente x gobierno y
'''
presidentes <- c("Fox", "Calderon", "Peña", " Obrador")
mandatos <- c("2000 - 2006", "2006 - 2012", "2012 - 2018", "2018 - 2024")

for (i in 1:length(presidentes)){
    cat("El presidente: ", presidentes[i], "gobernó en el perido: ", mandatos[i],'\n')
}