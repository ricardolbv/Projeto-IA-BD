library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)
library(plotrix)
library(RColorBrewer)
install.packages("RColorBrewer")
myPalette <- brewer.pal(5, "Set2") 


data <- read.csv("./dataset.txt", header = TRUE, sep = ";")
data

#Limpeza de dados
sum(is.na(data)) #Nenhum dado com atributo ausente
data <-data[,-1]

#Exploração estatistica -------------------
comDoenca = sum(data$chd == 1) #160 tiveram doença na coronaria
semDoenca = sum(data$chd == 0) #360 Não tiveram doença na coronaria

pie(c(comDoenca, semDoenca), c("Doença Coronária","Sem doença Coronária"), border = "white", col=myPalette,
    main = "Distribuição do dataset")

#Faixa de idade do dataset
ggplot(data, aes(x=age)) +  geom_histogram(color="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")

#Faixa de idade que tiveram doença coronaria tendo histórico familiar
coronary = filter(data, chd == 1)
ggplot(coronary, aes(x=age, fill = famhist)) +    geom_bar() +
  scale_x_binned() + labs(title = "Plot de doença coronária") +
  xlab("Idade") + ylab("Quantidade de pessoas") 

#Explorando features - Doença coronaria
boxplot(coronary$tobacco, coronary$obesity, coronary$ldl,
        names = c("Acumulativa de tabacco (kg)", "Nível de obesidade", "Colesterol (LDL)"),
        border = "brown",
        main = "Doença coronária",
        col = "orange")

