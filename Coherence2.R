
setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

library(textmineR)
library(igraph)
library(ggraph)
library(ggplot2)
library(tm)
library(udpipe)


load("historia_annotated_dataset.Rda")



dtf <- subset(x, upos %in% c("NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

head(dtf)


dtm <- document_term_matrix(x = dtf)

dtm <- dtm_remove_lowfreq(dtm, minfreq = 2)

head(dtm_colsums(dtm))

dtm <- dtm_remove_terms(dtm, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))



# https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25

vocabulary <- dtf$term[ dtf$term_freq > 1 & dtf$doc_freq < nrow(dtm) / 2 ]

k_list <- seq(2, 40, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, 
                     k = k, 
                     alpha = 30/k, 
                     beta = 0.1, 
                     burnin = 500, 
                     best = TRUE, 
                     iterations = 4000)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines


#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Оптимальное количество тем (k)") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,30,1)) + ylab("Когерентность тем")


#alpha = 50/k
