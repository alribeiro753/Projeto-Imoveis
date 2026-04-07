
# Leitura dos pacotes necessários

library(tidyverse)
library(ggplot2)
library(patchwork)

# Etapa 1: Leitura do banco de dados)

dados_imoveis = read.csv("imoveis.csv", sep = ";")
attach(dados_imoveis)


# Etapa 2: Gráficos

# Gráfico de densidade da variavel metragem em relação ao preço

bins_fd = nclass.FD(preco)
bins_scott = nclass.scott(preco)

met_relativo_preco = ggplot(dados_imoveis, aes(x = preco)) +
  geom_histogram(bins = bins_fd,
                 fill = "cyan4",
                 color = "black") +
  xlab("Preço") +
  ylab("Frequência absoluta") +
  theme_bw() +
  theme(text = element_text(size = 14))

met_relativo_preco                        
