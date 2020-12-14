# https://lse-me314.github.io/assignment11/ME314_assignment11.html

library(quanteda)
library(topicmodels)
library(lda)
library(LDAvis)
library(stm)
library(knitr)
set.seed(221186)

load("hoc_speeches.Rdata")


## Create a corpus of speeches
speechCorpus <- corpus(speeches$speech)

## Convert to dfm, removing some words that appear very regularly
speechDfm <- dfm(speechCorpus, remove = c(stopwords("english"), "will", "hon", "right","people","government","can","friend","house","gentleman","said", "interruption", "prime", "minister", "secretary", "state"), stem = F)

## Trim some rarely occuring words
speechDfm <- dfm_trim(speechDfm, min_termfreq = 15, min_docfreq = 0.0015, docfreq_type = "prop")






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


