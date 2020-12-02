# install.packages("RColorBrewer")
# install.packages("tidyverse")
# install.packages("RColorBrewer")

library(tidyr)
library(dplyr)
library(dslabs)
library(rpart.plot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(class)
library(rpart)
library(rpart.plot)
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

comDoenca = sum(data$chd == 1) # 160 tiveram doença na coronaria
semDoenca = sum(data$chd == 0) # 302 Não tiveram doença na coronaria

coronary = filter(data, chd == 1)
nonCoronary = filter(data, chd == 0)

# Explorando features - Doença coronaria
boxplot(coronary$tobacco, coronary$obesity, coronary$ldl)

doencaPlot <- pie(c(comDoenca, semDoenca), c("Doença Coronária","Sem doença Coronária"), border = "white", col=myPalette,
                  main = "Distribuição do dataset")

# Faixa de idade do dataset
idadePlot <-ggplot(data, aes(x=age)) +  geom_histogram(color="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")

# Faixa de idade que tiveram doença coronaria tendo histórico familiar
histPlot <- ggplot(coronary, aes(x=age, fill = famhist)) + geom_bar() +
  scale_x_binned()  +
  xlab("Idade") + ylab("Quantidade de pessoas") 

ggarrange(idadePlot, histPlot, labels = c("Faixa de idade do dataset", "Doença coronaria e histórico familiar"), nrow = 2)

# tranformando "Present" e "Absent" em dados númericos
data$famhist <- replace(data$famhist, data$famhist == "Present", "1") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "Absent", "2") # Atribuindo Absent com 2
data$famhist <- as.numeric(data$famhist)

# conferir balanceamentos das observações em prol do histórico familiar
famhist_present = sum(data$famhist == 1) # 270 tiveram doença na coronaria
famhist_ausent = sum(data$famhist == 2) # 192 Não tiveram doença na coronaria

# Padronizando o dataset com o tipo númerico para todas as colunas
data[1:462,c(1,6,9,10)] <- sapply(data[1:462,c(1,6,9,10)], as.numeric)
str(data)

# Ao gerar o boxplot de de distribuição de dados por todas features 
# vimos a necessidade de normalizar os dados e que famhist e chd poderiam ser separados para uma 
# visualização mais relevante.
boxplot(data) 

# separação e armazenamento das features chd e famhist
chd_feature <- data[10]
famhist_feature <- data[5]

# Novo data frame sem as features chd e famhist, features com potencial para classificador 
df <- data[-10]
df <- df[-5]

# dataset comtém com diferentes precisões decimais entre as features, aplicar escala
data_scale <- as.data.frame(scale(df))

# new_df é um data frame padronizado em escala mais as features chd e famhist
new_df <- cbind(data_scale, famhist_feature)
new_df <- cbind(new_df, chd_feature)

boxplot(data_scale)

# ------------------------------------------2 parte - IA --------------------------------------------------------
# Conjunto de treino e teste (80% 20%)
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

# Gráfico de pizza - quantidade de pacientes com resposta de doença cardíaca coronária
diagnostico_frequencia <-table(train$chd)
diagnostico_porcentagem <- round(prop.table(diagnostico_frequencia)*100)
diagnostico_porcentagem # '0'(sem a doença) -> 66% e '1' (com doença) -> 34%
diagnostico_tabela <-as.data.frame(diagnostico_porcentagem)
colnames(diagnostico_tabela)[1] <- "heart_attack"

ggplot(diagnostico_tabela, aes(x="", y= Freq, fill=heart_attack))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  theme(legend.position="none") +
  
  geom_text(aes(y = c(66, 20), label = Freq), color = "white", size=6) +
  scale_fill_brewer(palette="Set1") +
  labs(x=NULL, y=NULL, title="Presença '1' e Ausencia '0' de crise cardiaca")

# Plot geral das features
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

# PCA variavel com maior contribuição
fviz_pca_var(pca, geom=c("text","point"))

# Direção da variavel com maior contribuição por distribuição de individuos 
fviz_pca_biplot(pca,geom = "point", pointsize= 2,habillage = chd_feature$chd, alpha.ind = 1,
                select.var = list(contrib = 6),
                col.var = "black", # Variables color 
)

# Direção da variavel com maior contribuição por distribuição de individuos 
fviz_pca_biplot(pca,geom = "point", pointsize= 2,habillage = famhist_feature$famhist, alpha.ind = 1,
                col.var = "black", # Variables color
)

# Conjunto de dados com maior contribuição
data_cardiovascular <- data[, c("alcohol", "tobacco", "age", "adiposity", "obesity", "ldl", "chd")]

# Conjunto de treino e teste (80% 20%) - Dataset reduzido
set.seed(123)
smp_size <- floor(0.80 * nrow(data_cardiovascular))
train_ind <- sample(seq_len(nrow(data_cardiovascular)), size = smp_size)

train_reduced <- data_cardiovascular[train_ind, ]
test_reduced <- data_cardiovascular[-train_ind, ]

dim(train_reduced)
dim(test_reduced)

# Conjunto de treino e teste (80% 20%) - Dataset completo
set.seed(123)
smp_size <- floor(0.80 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)
dim(test)

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

# KNN aplicado ao dataset reduzido
# k = 1
verificaknn(train_reduced, test_reduced, 1, 7)
# k = 3
verificaknn(train_reduced, test_reduced, 3, 7)
# k = 5
verificaknn(train_reduced, test_reduced, 5, 7)
# k = 11
verificaknn(train_reduced, test_reduced, 11, 7)


# -------------------- Algoritmo de árvore de decisão --------------------------
modelo <- rpart(chd~., train_reduced, method = "class", control = rpart.control(minisplit = 1))
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
verificaDesicionTree(modelo, train_reduced, 7) # Utilizando modelo para predizer o data set de treino

p <- ggplot(data, aes(perimeter_worst, concave_points_worst, group=diagnosis, colour = diagnosis))
p + geom_point()