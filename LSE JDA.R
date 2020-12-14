# https://lse-me314.github.io/solutions/ME314_assignment10_solution.html

setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

library(quanteda)
library(topicmodels)
library(LDAvis)
library(stm)
library(knitr)
library(lda)
library(servr)
set.seed(221186)


#install.packages("devtools") # get devtools to install quanteda.corpora
#library(devtools)
#devtools::install_github("quanteda/quanteda.corpora")


library(quanteda.corpora)
library(quanteda.textmodels)
install.packages("quanteda.corpora")
install.packages("quanteda.textmodels") # Movie reviews

data(data_corpus_moviereviews, package = "quanteda.textmodels")

# prepare the texts
moviesDfm <- dfm(data_corpus_moviereviews, remove = stopwords("en"), stem = FALSE, remove_punct = T, remove_numbers = T)
moviesDfm <- dfm_trim(moviesDfm, min_termfreq = 5)

# MCMC and model tuning parameters:
K <- 20
G <- 2000
alpha <- 0.02
eta <- 0.02

# convert to lda format
moviesDfmlda <- convert(moviesDfm, to = "lda")
# fit the model
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = moviesDfmlda$documents, K = K, 
                                   vocab = moviesDfmlda$vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 10 minutes on Jack's iMac

library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))), 
                   theta = t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))), 
                   doc.length = ntoken(moviesDfm), 
                   vocab = featnames(moviesDfm), 
                   term.frequency = colSums(moviesDfm))

serVis(json, out.dir = "visColl", open.browser = TRUE)