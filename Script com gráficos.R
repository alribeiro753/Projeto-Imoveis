# Script com os gráficos

# Método para o cálculo do número de barros de Freedman-Diaconis
bins_fd = nclass.FD(metragem)

# Histograma de frequência relativa sobre a variável metragem
ggplot(dados_imoveis, aes(x = metragem)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),  
                 bins = bins_fd,
                 fill = "cyan4",
                 color = "black") +
  geom_text(stat = "bin", 
            bins = bins_fd,
            aes(
              y = after_stat(count / sum(count)),
              label = scales::percent(after_stat(count / sum(count)), accuracy = 0.1)), 
            vjust = -0.5, 
            size = 4) +
  xlab("Metragem") + 
  ylab("Frequência relativa (%)") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  theme(text = element_text(size = 14))



# Gráfico de dispersão entre as variáveis 'metragem' e 'preco'
ggplot(data = dados_imoveis, mapping = aes(x = metragem,
                                           y = preco))+
  geom_point(size = 2,
             shape = 19,
             color = "midnightblue")+
  geom_smooth()+
  stat_cor(method = "spearman",
           label.x = 90,
           label.y = 2050,
           size = 5) +
  xlab("Metragem")+
  ylab("Preço dos imóveis")+
  theme_bw()+
  theme(text = element_text(size = 14))


# Gráfico de dispersão entre as variáveis 'metragem' e 'imposto_anual'
ggplot(data = dados_imoveis, mapping = aes(x = metragem,
                                           y = imposto_anual))+
  geom_point(size = 2,
             shape = 19,
             color = "midnightblue")+
  geom_smooth()+
  stat_cor(method = "spearman",
           label.x = 90,
           label.y = 2050,
           size = 5) +
  xlab("Metragem")+
  ylab("Imposto anual")+
  theme_bw()+
  theme(text = element_text(size = 14))