# https://lse-me314.github.io/assignment11/ME314_assignment11.html

setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")


library(quanteda)
library(topicmodels)
library(lda)
library(LDAvis)
library(stm)
library(knitr)
set.seed(221186)

#load("hoc_speeches.Rdata")
load("historia_annotated_dataset.Rda")

## Create a corpus of speeches
lemmaCorpus <- corpus(x$lemma)

## Convert to dfm, removing some words that appear very regularly
lemmaDfm <- dfm(myCorpus)

## Trim some rarely occuring words
speechDfm <- dfm_trim(speechDfm, min_termfreq = 15, min_docfreq = 0.0015, docfreq_type = "prop")





# Create a term-document matrix
dtm <- as.matrix(dtm)
tdm <- t(dtm)

# Convert a DTM to a Character Vector of documents
library(textmineR)
dtm.to.docs <- textmineR::Dtm2Docs(dtm = dtm)


## Convert dtm to a list of text
dtm.to.docs <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert list of text to a Corpus

myCorpus <- VCorpus(VectorSource(dtm.to.docs))
inspect(myCorpus)

# Created term-document matrix from corpus

tdm <- TermDocumentMatrix(myCorpus)

td_matrix <- as.matrix(tdm)













# Convert to lda format
speechDfmlda <- convert(speechDfm, to = "lda")

#sdfm <- as.matrix(speechDfmlda)

# MCMC and model tuning parameters:
K <- 30 # Number of topics
G <- 2000 # Number of iterations
alpha <- 0.02 # Prior for topic proportions
eta <- 0.02 # Prior for topic distributions

# # Don't run!
# # Fit the model
t1 <- Sys.time() # Start timer

fit <- lda.collapsed.gibbs.sampler(documents = speechDfmlda$documents, K = K,
                                   vocab = speechDfmlda$vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time() # End timer

t2 - t1  # about 10 minutes on Jack's MacBook Pro


library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))), 
                   theta = t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))), 
                   doc.length = ntoken(speechDfm), 
                   vocab = colnames(speechDfm), 
                   term.frequency = colSums(speechDfm))
serVis(json, out.dir = "exampleVis", open.browser = TRUE)


