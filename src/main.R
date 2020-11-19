library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)


data <- read.csv("./dataset.txt", header = TRUE, sep = ";")
data

#Limpeza de dados
sum(is.na(data)) #Nenhum dado com atributo ausente
data <-data[,-1]

#Exploração estatistica
comDoenca = sum(data$chd == 1) #160 tiveram doença na coronaria
semDoenca = sum(data$chd == 0) #360 Não tiveram doença na coronaria

#Faixa de idade do dataset
ggplot(data, aes(x=age)) +  geom_histogram(color="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 