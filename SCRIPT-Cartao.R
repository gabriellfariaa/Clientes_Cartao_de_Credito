# K-Means Clustering - Clientes de cartão de crédito

# Carregando Pacotes
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra",
             "reshape2",
             "misc3d",
             "plot3D", 
             "cluster", 
             "factoextra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# K-Means algorithm is an unsupervised learning and clustering algorithm.


# Importando o dataset
data.info <- read.csv("cartao_credito.csv")

# Realizando a padronização dos dados
dados_padronizado <- as.data.frame(scale(data.info[,3:7]))

# Visualização
scatter3D(x=dados_padronizado$Avg_Credit_Limit,
          y=dados_padronizado$Total_Credit_Cards,
          z=dados_padronizado$Total_visits_bank,
          phi = 1, bty = "g", pch = 20, cex = 1,
          xlab = "Limite Médio",
          ylab = "Nº Cartões",
          zlab = "Nº Visitas",
          main = "Clientes", 
          colkey = F)

# Método de Elbow para identificação do número ótimo de clusters
dev.off()
fviz_nbclust(dados_padronizado, kmeans, method = "wss", k.max = 10)

## Podemos concluir que 4 clusters é uma opção viável

# Elaboração da clusterização não hieráquica k-means

cluster_kmeans <- kmeans(dados_padronizado,
                         centers = 4)

# Adicionando a variável

dados_padronizado$cluster_K <- factor(cluster_kmeans$cluster)
data.info$cluster_K <- factor(cluster_kmeans$cluster)

# Analisando por meio de gráficos 

ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_Credit_Cards, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Cartões")


ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_bank, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Visitas ao Banco")


ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_online, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Visitas ao Banco")


# Analisando por meio de estatísticas descritivas

análise <- group_by(data.info, cluster_K) %>%
  summarise(limite = mean(Avg_Credit_Limit, na.rm = TRUE),
            qtde_cartoes = mean(Total_Credit_Cards, na.rm = TRUE),
            qtde_visitas = mean(Total_visits_bank, na.rm = TRUE),
            qtde_online = mean(Total_visits_online, na.rm = TRUE),
            qtde_ligacao = mean(Total_calls_made, na.rm = TRUE))


library(ggplot2)
library(gridExtra)

# Gráfico 1
plot1 <- ggplot(dados_padronizado, aes(x = Avg_Credit_Limit, y = Total_Credit_Cards, color = cluster_K)) +
  geom_point() +
  labs(x = "Limite Médio", y = "Quantidade de Cartões")

# Gráfico 2
plot2 <- ggplot(dados_padronizado, aes(x = Avg_Credit_Limit, y = Total_visits_bank, color = cluster_K)) +
  geom_point() +
  labs(x = "Limite Médio", y = "Quantidade de Visitas ao Banco")

# Gráfico 3
plot3 <- ggplot(dados_padronizado, aes(x = Avg_Credit_Limit, y = Total_visits_online, color = cluster_K)) +
  geom_point() +
  labs(x = "Limite Médio", y = "Quantidade de Visitas Online ao Banco")

# Agrupar os gráficos
grid.arrange(plot1, plot2, plot3, nrow = 1)


