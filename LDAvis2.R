
# https://www.r-bloggers.com/2016/06/ldavis-show-case-on-r-bloggers/


setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")


library(RSQLite)
db.conn <- 
  dbConnect(
    dbDriver("SQLite"),
    "r-bloggers.db"
  )
posts <- dbGetQuery(db.conn, "SELECT text from posts")
dbDisconnect(db.conn)

## the following fragment of code in this section
## is motivated by
## http://cpsievert.github.io/LDAvis/reviews/reviews.html
# tokenize on space and output as a list:
doc.list <- strsplit(posts[, 1], "[[:space:]]+")
# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
# remove terms that occur fewer than 5 times:
term.table <- term.table[term.table > 5]
vocab <- names(term.table)

library(tm)
library(textmineR)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (3740)
W <- length(vocab)  # number of terms in the vocab (18,536)
doc.length <- sapply(documents, 
                     function(x) sum(x[2, ]))  
# number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  
# frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]



# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02
# Fit the model:
library(lda)
set.seed(456)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(
  documents = documents, K = K, 
  vocab = vocab, num.iterations = G, 
  alpha = alpha, eta = eta, 
  initial = NULL, burnin = 0,
  compute.log.likelihood = TRUE
)
t2 <- Sys.time()
t2 - t1  # about 7 seconds on my laptop
library(archivist)
saveToRepo(fit, repoDir = "Museum")




theta <- t(apply(fit$document_sums + alpha,
                 2,
                 function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta,
               2,
               function(x) x/sum(x)))


library(LDAvis)
library(servr)
# create the JSON object to feed the visualization:
json <- createJSON(
  phi = phi, 
  theta = theta, 
  doc.length = doc.length, 
  vocab = vocab, 
  term.frequency = term.frequency
)
serVis(json, out.dir = 'vis', 
       open.browser = FALSE)








# convert to matrix to allow row and column sums to be calculated
td.mat <- as.matrix(dtm)

topic.no <- 18

lda <- topicmodels::LDA(dtm, k = topic.no, method = "Gibbs")

phi <- posterior(lda)$terms
theta <- posterior(lda)$topics
doc.length <- rowSums(td.mat)
term.frequency <- colSums(td.mat)
vocab <- tm::Terms(dtm)

