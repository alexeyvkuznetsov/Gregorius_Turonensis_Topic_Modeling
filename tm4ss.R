# https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html

setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)

textdata <- read.csv("data/sotu.csv", sep = ";", encoding = "UTF-8")

sotu_corpus <- corpus(textdata$text, docnames = textdata$doc_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# extended stopword list
stopwords_extended <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Create a DTM (may take a while)
corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

sotu_collocations <- textstat_collocations(corpus_tokens, min_count = 25)
sotu_collocations <- sotu_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)


# Create DTM, but remove terms which occur in less than 1% of all documents
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = Inf, docfreq_type = "prop")

# have a look at the number of documents and terms in the matrix
dim(DTM)



top10_terms <- c( "unite_state", "past_year", "year_ago", "year_end", "government", "state", "country", "year", "make", "seek")

DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]






