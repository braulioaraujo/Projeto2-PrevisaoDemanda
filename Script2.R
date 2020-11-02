## Obs.: Achei o nível de dificuldade alto para o meu nível de conhecimento. Além disso,
## o custo computacional e o tempo dispendido seriam altos para mim. Decidi, então, realizar
## análises superficiais, apenas para treinar a exploração dos dados, etc.

## O objetivo da empresa é prever a demanda por produtos em cada ponto de venda. Desta forma, 
## o ideal é evitar solicitar produtos demais ou produtos de menos a cada semana. 
## É preciso considerar que os dois aspectos mais relevantes que queremos são: 

## 1- Evitar solicitar produtos em excesso
## 2- Solicitar produtos que atendam a demanda

library(dplyr)
library(ggplot2)

dt = read.csv(file = "train.csv")

# Exploração de dados

agencias = dt %>%
  group_by(Agencia_ID) %>%
  summarise(vendas = sum(Venta_hoy)) %>%
  arrange(desc(vendas))
agencias
summary(agencias)
ggplot(agencias, aes(vendas)) + geom_histogram()

canais = dt %>%
  group_by(Canal_ID) %>%
  summarise(vendas = sum(Venta_hoy)) %>%
  arrange(desc(vendas))
canais
ggplot(canais, aes(Canal_ID,vendas)) + geom_col()

rota = dt %>%
  group_by(Ruta_SAK) %>%
  summarise(vendas = sum(Venta_hoy)) %>%
  arrange(desc(vendas))
rota
ggplot(rota, aes(vendas)) + geom_histogram()

produtos = dt%>%
  group_by(Producto_ID) %>%
  summarise(vendas = sum(Venta_hoy)) %>%
  arrange(desc(vendas))
produtos
summary(produtos)
ggplot(produtos, aes(vendas)) + geom_histogram()

cliente = dt %>%
  group_by(Cliente_ID) %>%
  summarise(vendas = sum(Venta_hoy)) %>%
  arrange(desc(vendas))
cliente
summary(cliente)
ggplot(cliente, aes(vendas)) + geom_density()
cliente[1,2]/sum(cliente$vendas)

dtx = dt %>% 
  filter(Semana==3) %>%
  arrange(desc(Venta_hoy))
dtx = dtx[1:100,]

ggplot(dtx, aes(x=Venta_hoy, y=Agencia_ID, color=Canal_ID)) 
          + geom_point() + scale_x_log10()

# Filtro no dataset considerando a "agência" 1110, e cada semana

dt3 = dt %>%
  filter(Semana == 3 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt4 = dt %>%
  filter(Semana == 4 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt5 = dt %>%
  filter(Semana == 5 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt6 = dt %>%
  filter(Semana == 6 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt7 = dt %>%
  filter(Semana == 7 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt8 = dt %>%
  filter(Semana == 8 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

dt9 = dt %>%
  filter(Semana == 9 & Agencia_ID == 1110) %>%
  mutate(Pedidos = Venta_uni_hoy - Dev_uni_proxima, lucroliquido = Venta_hoy - Dev_proxima) %>%
  group_by(Producto_ID) %>%
  summarise(lucro = sum(lucroliquido), pedidosliq = sum(Pedidos), pedidos = sum(Venta_uni_hoy))

# Avaliando a série histórica de alguns produtos

semanas = c(3,4,5,6,7,8,9)

produto_73 = rbind(filter(dt3, Producto_ID == 73), filter(dt4, Producto_ID == 73), 
                   filter(dt5, Producto_ID == 73), filter(dt6, Producto_ID == 73),
                   filter(dt7, Producto_ID == 73), filter(dt8, Producto_ID == 73),
                   filter(dt9, Producto_ID == 73))
produto_73

produto_641 = rbind(filter(dt3, Producto_ID == 641), filter(dt4, Producto_ID == 641), 
                   filter(dt5, Producto_ID == 641), filter(dt6, Producto_ID == 641),
                   filter(dt7, Producto_ID == 641), filter(dt8, Producto_ID == 641),
                   filter(dt9, Producto_ID == 641))
produto_641

produto_739 = rbind(filter(dt3, Producto_ID == 739), filter(dt4, Producto_ID == 739), 
                   filter(dt5, Producto_ID == 739), filter(dt6, Producto_ID == 739),
                   filter(dt7, Producto_ID == 739), filter(dt8, Producto_ID == 739),
                   filter(dt9, Producto_ID == 739))
produto_739

produto_1125 = rbind(filter(dt3, Producto_ID == 1125), filter(dt4, Producto_ID == 1125), 
                   filter(dt5, Producto_ID == 1125), filter(dt6, Producto_ID == 1125),
                   filter(dt7, Producto_ID == 1125), filter(dt8, Producto_ID == 1125),
                   filter(dt9, Producto_ID == 1125))  
produto_1125
  
produto_1187 = rbind(filter(dt3, Producto_ID == 1187), filter(dt4, Producto_ID == 1187), 
                     filter(dt5, Producto_ID == 1187), filter(dt6, Producto_ID == 1187),
                     filter(dt7, Producto_ID == 1187), filter(dt8, Producto_ID == 1187),
                     filter(dt9, Producto_ID == 1187))  
produto_1187

produto_45183 = rbind(filter(dt3, Producto_ID == 45183), filter(dt4, Producto_ID == 45183), 
                     filter(dt5, Producto_ID == 45183), filter(dt6, Producto_ID == 45183),
                     filter(dt7, Producto_ID == 45183), filter(dt8, Producto_ID == 45183),
                     filter(dt9, Producto_ID == 45183))    
produto_45183

library(gridExtra)

g1 = ggplot(produto_73, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 73")
g2 = ggplot(produto_641, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 641")
g3 = ggplot(produto_739, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 739")
g4 = ggplot(produto_45183, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 45183")
g5 = ggplot(produto_1125, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 1125")
g6 = ggplot(produto_1187, aes(x=semanas,y=lucro)) + geom_line() + labs(title="Produto 1187")

grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3)

## Iremos avaliar qual a semana que gerou maior lucro, afinal, provavelmente são as semanas 
## que tinham produtos suficientes para atender a necessidade dos clientes.

dt$receitaliquida = dt$Venta_hoy - dt$Dev_proxima

receitasemana = dt %>%
  group_by(Semana) %>%
  summarise(receitaliquida = sum(receitaliquida))
receitasemana
ggplot(receitasemana, aes(x=Semana, y=receitaliquida)) + geom_col()

  
# Construindo o modelo

## Treinei um modelo com as variáveis CLiente_ID, Producto_ID, Agencia ID e Canal_ID. Devido
## ao alto custo computacional, extraí do modelo as duas variáveis de maior importância que
## foram Cliente_ID e Producto_ID, permitindo que eu elevasse a quantidade de instâncias no
## treinamento. Utilizei o modelo$variable.importance.
  
dtz = dt %>%
  filter(Semana == 4)

dtz$Agencia_ID = as.factor(dtz$Agencia_ID)
dtz$Canal_ID = as.factor(dtz$Canal_ID)
dtz$Cliente_ID = as.factor(dtz$Cliente_ID)
dtz$Producto_ID = as.factor(dtz$Producto_ID)
  
amostra = sample(2,11009510,replace=T, prob=c(0.01,0.1))
treino = dtz[amostra==1,]
teste = dtz[amostra==2,]

library(rpart)

modelo = rpart(Demanda_uni_equil ~ Cliente_ID + Producto_ID, 
               treino, method = "anova")

previsoes = predict(modelo, teste)
balanco = cbind(previsoes, teste$Demanda_uni_equil)

# Calculando R Squared

SSE = sum((balanco[2] - teste$Demanda_uni_equil)^2)
SST = sum((mean(dtz$Demanda_uni_equil) - teste$Demanda_uni_equil)^2)
R2 = 1 - (SSE/SST)
R2

# Calculando o erro médio

mse <- mean((teste$Demanda_uni_equil - balanco[2])^2)
print(mse)

rmse <- mse^0.5
rmse