
################################################################################
####                     Escalonamento Multidimensional (MASS)
################################################################################

#' # Carregar pacotes
library(MASS)    # Escalonamento
library(ggplot2) # Gráficos
library(smacof)  # Escalonamento
library(vegan)


#' # Carregar dados
dados <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
dados
attach(dados)


#' # Convertendo em matrizes
MNZ <- as.matrix(dados)  # Convertendo os dados em um objeto do tipo matrix
dist.MNZ <- as.dist(MNZ) # Convertendo em uma matriz diagonal inferior
dist.MNZ
cidades <- row.names(MNZ) # Nomes das cidades


# Verifica a estrutura de dados
str(MNZ)


#' # Escalonamento multidimensional não-métrico de Kruskal
#' # Calculando o estresse (Qualidade do ajuste. Quanto MENOR, melhor)
EMD1 <- isoMDS(dist.MNZ)    # Estresse atingido neste caso 7.3% (i.e 0.073)
EMD1$stress


#' ## Mudando o número de iterações e nível de tolerância
EMD2 <- isoMDS(dist.MNZ,tol=1e-6,maxit=200)  
EMD2$stress           # O estresse (stresse de Kruskal) é aproximad. 4% ou 0.04
EMD2$points           # NMDS coordenadas 


# As coordenadas produzidas pelo escalonamento multidimensional não são as mesmas que
# aquelas mostradas na Tabela 11.4, mas sua configuração no espaço bidimensional
# são semelhantes após os sinais em EMD2$points serem invertidos. Isso é visto
# no mapa produzido abaixo:


#' # Gráficos
# Preparação dos dados
points <- as.data.frame(-EMD2$points) # O sinal negativo antes do argumento inverte as posicoes no grafico
names(points) <- c("Dimensão1", "Dimensão2")
points$cidades <- cidades


# Avaliando
MDS2NZiso.Shep <- Shepard(dist.MNZ, EMD2$points, p = 2)
shep_points <- data.frame(DistânciaConfiguração = MDS2NZiso.Shep$yf, DissimilaridadeDados = MDS2NZiso.Shep$x)

p1 <- ggplot(shep_points, aes(x = DistânciaConfiguração, y = DissimilaridadeDados)) +
  geom_point(shape = 15, size = 3) +
  geom_line(color = "red") +
  ggtitle("Gráfico de Shepard") +
  xlab("Distâncias de Configuração") +
  ylab("Dissimilaridades dos Dados")+
  theme_bw()

print(p1)

ggsave("p1.png", dpi = 300)

# Mapa das cidades da Ilha Sul da NZ usando isoMDS
p2 <- ggplot(points, aes(x = Dimensão1, y = Dimensão2)) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = cidades), vjust = -1, size = 3) +
  ggtitle("Mapa das cidades da Ilha Sul da NZ \n Escalonamento Multidimensional Não Métrico usando isoMDS") +
  xlab("Dimensão 1") +
  ylab("Dimensão 2") +
  annotate("text", x = 200, y = -175, label = paste("Stress = ", round(EMD2$stress/100, 3)))+
  theme_bw()
print(p2)

ggsave("p2.png", dpi = 300)

#' # Usando o pacote smacof

# O método padrão em smacof é PCO (type="ratio"). Para NMDS não métrico use
# type="ordinal. Por padrão, ndim=2

EMD3 <- mds(dist.MNZ, type="ordinal")
EMD3