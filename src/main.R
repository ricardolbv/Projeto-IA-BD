library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)


data <- read.csv("./dataset.txt", header = TRUE, sep = ";")
data

#Limpeza de dados
sum(is.na(data)) #Nenhum dado com atributo ausente