
library(devtools)
devtools::install_github("cpsievert/LDAvisData")


library(LDAvis)
library(LDAvisData)
# LDAvisData can be installed from GitHub via 'devtools::install_github("cpsievert/LDAvisData")'
data(reviews, package = "LDAvisData")



# read in some stopwords:
library(tm)
stop_words <- stopwords("SMART")

# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)






# https://github.com/christopherlovell/speech-analysis/blob/master/topicmodelling.R
# filter out low scoring tf-idf terms
tfidf.scores <- colSums(as.matrix(tm::weightTfIdf(dtm)))
dtm <- dtm[,tfidf.scores > quantile(tfidf.scores, 0.3)]

# convert to matrix to allow row and column sums to be calculated
td.mat <- as.matrix(dtm)

topic.no <- 28

lda <- topicmodels::LDA(dtm, k = topic.no, method = "Gibbs")

phi <- posterior(lda)$terms
theta <- posterior(lda)$topics
doc.length <- rowSums(td.mat)
term.frequency <- colSums(td.mat)
vocab <- tm::Terms(dtm)


LDAvis.json <- LDAvis::createJSON(phi = phi,
                                  theta = theta,
                                  doc.length = doc.length,
                                  vocab = vocab,
                                  term.frequency = term.frequency)

LDAvis::serVis(LDAvis.json)

save(LDAvis.json, file = "ldavis.RData")

#save.image("image.RData")
#load("image.RData")

#rm(phi,theta,doc.length,term.frequency,vocab,not,lda,LDAvis.json,td.mat)
#write.table(LDAvis.json[[1]],file = "topics.json")


