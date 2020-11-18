library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)


data <- read.csv("./dataset.txt", header = TRUE, sep = ";")
data