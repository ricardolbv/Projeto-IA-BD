# install.packages("RColorBrewer")
# install.packages("tidyverse")
# install.packages("RColorBrewer")

library(tidyr)
library(dplyr)
library(dslabs)
library(rpart.plot)
library(tidyverse)  
library(cluster)    
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(class)
library(rpart)
library(rpart.plot)
library(reshape2)
myPalette <- brewer.pal(5, "Set2") 

data <- read.csv("./dataset.txt", sep =";", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), header = TRUE)

# -------------------- Parte 1 - Entendimento prévio e limpeza do dataset ----------------------------------
View(head(data, n=5))
dim(data) # 462 linhas e 11 colunas (sendo uma coluna identificador)
str(data)

# Verificar dados vazios em cada coluna e contabilizar
View(sapply(data, function(x) sum(is.na(x))))
sum(is.na(data))

# retirar coluna de identificação
data <- data[-1]

# tranformando "Present" e "Absent" em dados númericos
data$famhist <- replace(data$famhist, data$famhist == "Present", "1") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "Absent", "2") # Atribuindo Absent com 2
data$famhist <- as.numeric(data$famhist)

# Padronizando o dataset com o tipo númerico para todas as colunas
data[1:462,c(1,6,9,10)] <- sapply(data[1:462,c(1,6,9,10)], as.numeric)
str(data)

comDoenca = sum(data$chd == 1) # 160 tiveram doença na coronaria
semDoenca = sum(data$chd == 0) # 302 Não tiveram doença na coronaria

# Gráfico de pizza: Doentes e não doentes
pie_dataframe <- data.frame(
  group = c("Doente", "Não doente"),
  value = c(sum(data$chd == 1), sum(data$chd == 0)) # 160 tiveram doença na coronária e 302 Não tiveram doença na coronária
)

pie_dataframe_rate <- ggplot(pie_dataframe, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette="Blues") + theme_minimal() + 
  geom_text(aes(x = 1,label = paste(round(value / sum(value) *100,1),"%"), family = "sans"),
            position = position_stack(vjust = 0.5), size = 10)+
  labs(fill = "Classificação",
       x = NULL,
       y = NULL,
       title = "Pie: Doentes e não doentes",
       subtitle = "Em porcentagem") +
  theme_bw(base_size = 20, base_family = "mono")

pie_dataframe_no_rate <- ggplot(pie_dataframe, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette="Blues") + theme_minimal() + 
  geom_text(aes(x = 1,label = value, family = "sans"),
            position = position_stack(vjust = 0.5), size = 10)+
  labs(fill = "Classificação",
       x = NULL,
       y = NULL,
       title = "Pie: Doentes e não doentes",
       subtitle = "Em dados brutos") +
  theme_bw(base_size = 20, base_family = "mono")

ggarrange(pie_dataframe_no_rate, pie_dataframe_rate, ncol = 2)

# conferir balanceamentos das observações em prol do histórico familiar
famhist_present = sum(data$famhist == 1) # 158 tiveram doença na coronariana
famhist_ausent = sum(data$famhist == 2) # 214 Não tiveram doença na coronariana

# tranformando "Present" e "Absent" em dados númericos
data$famhist <- as.character(data$famhist)
data$famhist <- replace(data$famhist, data$famhist == "1", "Present") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "2", "Absent") # Atribuindo Absent com 2

# Filtro exemplares que tem e não tem resposta a doença cardiaca
coronary = filter(data, chd == 1)
nonCoronary = filter(data, chd == 0)

# Faixa de idade do dataset
ageCoronary <- ggplot(coronary, aes(x=age)) +  
  geom_histogram(color="black", fill="red", bins = 30, binwidth = 1)+ geom_density(fill="#FF6666") + 
  scale_x_continuous(name = "Idade") +
  scale_y_continuous(name = "Pessoas")

ageNoncoranary <- ggplot(nonCoronary, aes(x=age)) +  
  geom_histogram(color="black", fill="blue", bins = 30, binwidth = 1)+ geom_density(fill="#FF6666") +
  scale_x_continuous(name = "Idade") +
  scale_y_continuous(name = "Pessoas")

ggarrange(ageCoronary, ageNoncoranary, labels = c("Faixa de idade dos doentes", "Faixa de idade dos não doentes"), nrow = 2)

# Quantos dos exemplares que desenvolveram a doença possuiam historico familiar
ageCoronary <- ggplot(coronary, aes(x=age, fill=famhist)) +  
  geom_histogram(color="black", bins = 30, binwidth = 1)+ geom_density(fill="#FF6666") + 
  labs(fill = "Histórico familiar:",
       x = "idade",
       y = "Pessoas",
       title = "Doentes") +
  theme_classic(base_size = 20, base_family = "mono")

ageNoncoranary <- ggplot(nonCoronary, aes(x=age, group = famhist, fill=famhist)) + 
  geom_histogram(color="black", bins = 30, binwidth = 1)+ geom_density(fill="#FF6666") +
  labs(fill = "Histórico familiar:",
       x = "idade",
       y = "Pessoas",
       title = "Não Doentes") +
  theme_classic(base_size = 20, base_family = "mono")

ggarrange(ageCoronary, ageNoncoranary, nrow = 2)

# Comparando as principais comorbidades x idade: tobacco, adiposity and alcohol
comorbity <- data
comorbity$chd <- replace(comorbity$chd, comorbity$chd == 1, "Doente")
comorbity$chd <- replace(comorbity$chd, comorbity$chd == 0, "Não doente")

cmb_adiposity <- ggplot(comorbity, aes(x=age, y=adiposity, color = chd)) +
  geom_point(size = 4, show.legend = FALSE) +
  theme_classic2(base_size = 20, base_family = "mono")+
  labs(x = "Idade", y = "Adiposidade localizada")

cmb_tobacco <- ggplot(comorbity, aes(x=age, y=tobacco, color = chd)) +
  geom_point(size = 4) +
  theme_classic2(base_size = 20, base_family = "mono")+
  labs(color = "Classificação", x = "Idade", y = "tabaco cumulativo(kg)")

cmb_alcohol <- ggplot(comorbity, aes(x=age, y=alcohol, color = chd)) +
  geom_point(size = 4) +
  theme_classic2(base_size = 20, base_family = "mono") +
  labs(color = "Classificação", x = "Idade", y = "Consumo de álcool")

cmb_obesity <- ggplot(comorbity, aes(x=age, y=obesity, color = chd)) +
  geom_point(size = 4, show.legend = FALSE) +
  theme_classic2(base_size = 20, base_family = "mono") +
  labs(x = "Idade", y = "Obesidade")

# Agrupando comparações
ggarrange(cmb_obesity, cmb_alcohol, cmb_adiposity, cmb_tobacco, nrow = 2, ncol = 2, common.legend = TRUE, legend = "top")

# Analisando a comorbidade mental
ggplot(comorbity, aes(x=age, y=typea, color = chd)) +
  geom_point(size = 5) +
  theme_classic2(base_size = 20, base_family = "mono") +
  labs(x = "Idade", y = "typea", title = "Comorbidade mental x idade", color = "Classificação")

data$famhist <- replace(data$famhist, data$famhist == "Present", "1") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "Absent", "2") # Atribuindo Absent com 2
data$famhist <- as.numeric(data$famhist)

# separação e armazenamento das features chd e famhist
chd_feature <- data[10]
famhist_feature <- data[5]

# Novo data frame sem as features chd e famhist, features com potencial para classificador 
df <- data[-10]
df <- df[-5]

# dataset comtém com diferentes precisões decimais entre as features, aplicar escala
data_scale <- as.data.frame(scale(df))

# new_df é um data frame padronizado em escala mais as features chd e famhist
new_df_scale <- cbind(data_scale, famhist_feature)
new_df_scale <- cbind(new_df_scale, chd_feature)
View(new_df_scale)

# Comparando features com boxplot entre quem teve ou não a doença
data_mt <- new_df_scale[-9]
data_mt <- melt(new_df_scale[-9], id.vars = c("chd"))
data_mt$chd <- replace(data_mt$chd, data_mt$chd == 1, "Doente")
data_mt$chd <- replace(data_mt$chd, data_mt$chd == 0, "Não doente")

ggplot(data_mt, aes(x=chd,y=value,fill=chd)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable) + 
  labs(title = "Boxplot: Features normalizadas",fill = "Classificação", x= element_blank(), y = element_blank())

# ------------------------------------------2 parte - IA --------------------------------------------------------

# Conjunto de treino e teste (80% 20%) - Dados original
set.seed(123)
smp_size <- floor(0.80 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)
dim(test)

# Verificando se o dataset de treino é classificavel e possibilita exploração.

# preparando chd para colorir plot
chd <- as.factor(train$chd)

# Comparativo de todas 
plot(train, col = chd)

# É possivél ver um excesso de gordura no organismo ao longo da idade e também o aumento de numero 
# de paciantes com resultado de doença coronária. 
plot(x = train$age, y = train$adiposity, col = chd, pch =19, main="Adiposidade x Idade",
     ylab="Adiposidade", xlab="Idade")

# ------------------- Redução de dimencionalidade ------------------------------

# Análise de variância de cada uma das colunas (var). 
# Alguma coluna apresenta variância muito menor do que outras? Se sim, quantas e quais?
variance <- sapply(df, var)
summary(variance) # Todas colunas contém algum tipo de variancia

low_variance <- variance[unlist(variance >= 0 & variance <= 18)]
length(low_variance)

# Aplicando PCA para verificar a sugestão de redução de dimensão de forma estatistica
pca <- prcomp(df, center = TRUE, scale. = TRUE) # 8 Componentes principais
summary(pca)

# plot porcentagem de explicação de variancias por dimensões
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 10))

# Resultados da análise de componentes principais para variáveis
var <- get_pca_var(pca)
var

# PCA Inividuos por classe de numeros
fviz_pca_ind(pca, geom="point", pointsize= 3, habillage = chd_feature$chd, alpha.ind = 1)


# Direção da variavel com maior contribuição por distribuição de individuos 
fviz_pca_biplot(pca,geom = "point", pointsize= 2,habillage = chd_feature$chd, alpha.ind = 1,
                select.var = list(contrib = 6),
                col.var = "black", # Variables color 
)

# Direção da variavel com maior contribuição por distribuição de individuos 
fviz_pca_biplot(pca,geom = "point", pointsize= 2,habillage = famhist_feature$famhist, alpha.ind = 1,
                col.var = "black", # Variables color
)

# ----------------------- Algoritmo KNN com k = 1,3,5 e 11 vizinhos ------------------------------
# train[,10] representa coluna chd
verificaknn <- function(datasetTrain, datasetTest, vetorK, posicaoClassificador){
  classesTrain <- datasetTrain[ ,posicaoClassificador]
  datasetTrain <- datasetTrain[ , -posicaoClassificador]
  
  classesTest <- datasetTest[ , posicaoClassificador]
  datasetTest <- datasetTest[ , -posicaoClassificador]
  
  result <- knn(datasetTrain, datasetTest, classesTrain, vetorK)
  
  # Matriz de confusão
  print("Matriz confusão:")
  #print(as.matrix(table(classesTest, result)))
  matriz <- as.matrix(table(classesTest, result))
  
  # Indíce de acerto
  acc <- sum(diag(matriz))/nrow(datasetTest)
  print("Indice de acerto:")
  print(acc)
}

# KNN com dataset sem redução
# k = 1
verificaknn(train, test, 1, 10)
# k = 3
verificaknn(train, test, 3, 10)
# k = 5
verificaknn(train, test, 5, 10)
# k = 11
verificaknn(train, test, 11, 10)

# -------------------- Algoritmo de árvore de decisão --------------------------
modelo <- rpart(chd~., train, method = "class", control = rpart.control(minisplit = 1))
plot <- rpart.plot(modelo, type = 3)

verificaDesicionTree <- function(modelo, datasetTest, posicaoClassificador){
  classesTest <- datasetTest[ , posicaoClassificador]
  datasetTest <- datasetTest[ , -posicaoClassificador]
  
  pred <- predict(modelo, datasetTest, type = "class")
  
  # Matriz de confusão
  print("Matriz confusão:")
  matriz <- as.matrix(table(classesTest, pred))
  print(matriz)
  
  # Indíce de acerto
  acc <- sum(diag(matriz))/nrow(datasetTest)
  print("Indice de acerto:")
  print(acc)
}

verificaDesicionTree(modelo, test, 10)
verificaDesicionTree(modelo, data, 10) # Utilizando o modelo para predizer todo o data set
