library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)
library(plotrix)
library(RColorBrewer)
library(ggpubr)
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

#p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
#  geom_boxplot()


NdoencaFeaturesPlot <- boxplot(nonCoronary$tobacco, nonCoronary$obesity, nonCoronary$ldl,
                              names = c("Acumulativa de tabacco (kg)", "Nível de obesidade", "Colesterol (LDL)"),
                              border = "brown",
                              main = "Sem doença coronária",
                              col = "orange")
