# K-Means Clustering - Clientes de cartão de crédito
Este repositório contém um projeto de análise de dados utilizando o algoritmo de K-Means Clustering para categorizar os clientes de uma operadora de cartão de crédito. O conjunto de dados utilizado é uma amostra de informações de cartão de crédito dos clientes, que podem ser usadas para identificar clientes mais fiéis.

## Conjunto de Dados
O conjunto de dados utilizado pode ser encontrado em: Credit Card Customer Data

As características presentes nos dados são as seguintes:

- Avg_Credit_Limit: Limite médio do cartão de crédito para o cliente
- Total_Credit_Cards: Total de cartões de crédito de propriedade do cliente
- Total_visits_bank: Número total de visitas ao banco pelo cliente
- Total_visits_online: Total de visitas online do cliente ao banco
- Total_calls_made: Total de ligações efetuadas pelo cliente ao banco

![image](https://github.com/gabriellfariaa/Clientes_Cartao_de_Credito/blob/main/Data.info.png?raw=true)
![image](https://github.com/gabriellfariaa/Clientes_Cartao_de_Credito/blob/main/Clientes.png?raw=true)

## Objetivo
O objetivo deste projeto é categorizar os clientes da operadora de cartão de crédito com base nas características fornecidas. Utilizando o algoritmo de K-Means Clustering, os clientes serão agrupados em clusters com base em suas similaridades.

## Implementação
O código-fonte para implementação do K-Means Clustering nos dados de clientes de cartão de crédito pode ser encontrado em: Script-Cartao.R

## Instruções
Baixe o conjunto de dados de Credit Card Customer Data e salve-o na pasta do projeto.
Fonte: https://www.kaggle.com/datasets/aryashah2k/credit-card-customer-data

Execute o arquivo Script-Cartao.R para realizar a categorização dos clientes utilizando o algoritmo de K-Means Clustering.
Analise os resultados obtidos e as categorias formadas para melhor compreensão dos perfis dos clientes.
Sinta-se à vontade para explorar o código e adaptá-lo às suas necessidades específicas.

## Identificação numero de Cluster = 4

![image](https://github.com/gabriellfariaa/Clientes_Cartao_de_Credito/blob/main/Elbow.png?raw=true)


## Analisando por meio de estatísticas descritivas
![image](https://github.com/gabriellfariaa/Clientes_Cartao_de_Credito/blob/main/Analise%20descritiva.png?raw=true)

- Clientes com Avg_Limite > ~45.000, que talvez clientes fiéis façam menos chamadas (0-2) do que os clientes com Avg_Limit ~10.000 a 15.000;
- Clientes com 1-4 Cartões de Crédito fazem mais de 5 chamadas (4-8)
- Clientes que visitam bancos com menos frequência fazem mais ligações.

![image](https://github.com/gabriellfariaa/Clientes_Cartao_de_Credito/blob/main/conclus%C3%A3o.png?raw=true)
