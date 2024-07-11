
################################################################################
####                     Escalonamento Multidimensional
####                     Usando o pacote "Vegan"
################################################################################
library(vegan)

#' # Carregar dados
dados <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
dados
attach(dados)

#' # Usando o pacote vegan
(EMD4 <- metaMDS(MNZ, distance="euclidean", autotransform=FALSE))
stress.dim2 <- EMD4$stress
scores.sites <- scores(EMD4,display="sites")

detach(dados)
