
################################################################################
####                     Escalonamento Multidimensional (smacof)
################################################################################

#' # Carregar pacotes
library(smacof)  # Escalonamento

dados <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
dados
attach(dados)

# O método padrão em smacof é PCO (type="ratio"). Para NMDS não métrico use
# type="ordinal. Por padrão, ndim=2

EMD3 <- mds(dist.MNZ, type="ordinal")
EMD3

detach(dados)
