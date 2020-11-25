# library(tidyr)
# library(dplyr)
# library(dslabs)
# library(rpart.plot)
# library(tidyverse)  # data manipulation
# # install.packages("tidyverse")
# library(cluster)    # clustering algorithms
# library(factoextra)
# library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
# install.packages("RColorBrewer")
library(ggpubr)
install.packages("RColorBrewer")

myPalette <- brewer.pal(5, "Set2") 

data <- read.delim("./cardiovascular.txt", sep =";", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), header = TRUE)

#  ------------------- Entendimento previo e limpeza do dataset ----------------------------------
View(head(data, n=5))
dim(data) # 462 linhas e 11 colunas (sendo uma coluna identificador)
str(data)

# Verificar dados vazios em cada coluna e contabilidar
View(sapply(data, function(x) sum(is.na(x))))
sum(is.na(data))

# retirar coluna de identificação
data <- data[-1]


# tranformando "Present" e "Absent" em dados númerico
data$famhist <- replace(data$famhist, data$famhist == "Present", "1") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "Absent", "2") # Atribuindo Absent com 2
data$famhist <- as.numeric(data$famhist)

# Padronizando o dataset com o tipo numerico para todas as colunas
data[1:462,c(1,6,9,10)] <- sapply(data[1:462,c(1,6,9,10)], as.numeric)
str(data)

# Ao gerar o boxplot de de distribuição de dados por todas features 
# vimos a necessidade de normalizar os dados.
boxplot(data) 

chd_feature <- data[10]
famhist_feature <- data[5]

df <- data[-10]
df <- df[-5]

# dataset com muitas features, aplicar escala
data_scale <- as.data.frame(scale(df))

new_df <- cbind(data_scale, famhist_feature)
new_df <- cbind(new_df, chd_feature)

boxplot(df)
View(new_df)

# --------------------- Conjunto de treino e teste (80% 20%) ------------------------------------
set.seed(123)
smp_size <- floor(0.80 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)
dim(test)

# preparando chd para colorir plot
chd <- as.factor(train$chd)

# ----------------- Exploração inicial dos dados ------------------------------
comDoenca = sum(data$chd == 1) # 160 tiveram doença na coronaria
semDoenca = sum(data$chd == 0) # 360 Não tiveram doença na coronaria

famhist_present = sum(data$famhist == 1) # 160 tiveram doença na coronaria
famhist_ausent = sum(data$famhist == 2) # 360 Não tiveram doença na coronaria

# Faixa de idade do dataset
ggplot(train, aes(x=age)) +  geom_histogram(color="black", fill="white", bins = 30)+ geom_density(alpha=.2, fill="#FF6666")

# Faixa de idade que tiveram doença coronaria tendo histórico familiar
coronary = filter(train, chd == 1)

ggplot(coronary, aes(x=age, fill = famhist)) +    geom_bar() +
  scale_x_binned() + labs(title = "Plot de doença coronária") +
  xlab("Idade") + ylab("Quantidade de pessoas") 

# Explorando features - Doença coronaria
boxplot(coronary$tobacco, coronary$obesity, coronary$ldl,

doencaPlot <- pie(c(comDoenca, semDoenca), c("Doença Coronária","Sem doença Coronária"), border = "white", col=myPalette,
    main = "Distribuição do dataset")

#Faixa de idade do dataset
idadePlot <-ggplot(data, aes(x=age)) +  geom_histogram(color="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")


#Faixa de idade que tiveram doença coronaria tendo histórico familiar
coronary = filter(data, chd == 1)
nonCoronary = filter(data, chd == 0)

histPlot <- ggplot(coronary, aes(x=age, fill = famhist)) +    geom_bar() +
  scale_x_binned()  +
  xlab("Idade") + ylab("Quantidade de pessoas") 

ggarrange(idadePlot, histPlot, labels = c("Faixa de idade do dataset", "Doença coronaria e histórico familiar"), nrow = 2)

#Explorando features - Doença coronaria
doencaFeaturesPlot <- boxplot(coronary$tobacco, coronary$obesity, coronary$ldl,

        names = c("Acumulativa de tabacco (kg)", "Nível de obesidade", "Colesterol (LDL)"),
        border = "brown",
        main = "Doença coronária",
        col = "orange")


# Plot geral das features
plot(train, col = chd)

# É possivél ver um excesso de gordura no organismo ao longo da idade e também o aumento de numero 
# de paciantes com resultado de doença coronária. 
plot(x = train$age, y = train$adiposity, col = chd, pch =19, main="Adiposidade x Idade",
     ylab="Adiposidade", xlab="Idade")


pie(c(comDoenca, semDoenca), c("Doença Coronária","Sem doença Coronária"), border = "white", col=myPalette,
    main = "Distribuição do dataset")

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
#p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
#  geom_boxplot()


NdoencaFeaturesPlot <- boxplot(nonCoronary$tobacco, nonCoronary$obesity, nonCoronary$ldl,
                              names = c("Acumulativa de tabacco (kg)", "Nível de obesidade", "Colesterol (LDL)"),
                              border = "brown",
                              main = "Sem doença coronária",
                              col = "orange")

