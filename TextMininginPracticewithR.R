
setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

library(tm)
library(qdap)
library(lda)
library(GuardianR)
library(pbapply)
library(LDAvis)
library(treemap)
library(car)
options(stringsAsFactors = F)
text<-read.csv('Guardian_articles_11_14_2015_12_1_2015.csv')


phi <- posterior(fitted)$terms %>% as.matrix
theta <- posterior(fitted)$topics %>% as.matrix
vocab <- colnames(phi)
doc_length <- vector()
