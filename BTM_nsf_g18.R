# USAspending Exploration: NSF, FY 18 grants.  Topic Modeling on titles using Biterm Topic Modeling (BTM).

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)

library(udpipe)
library(data.table)
library(stopwords)
library(BTM)
library(textplot)
library(ggraph)
library(LDAvis)

#
# DATA INGESTION: NSF FY18 grants ------------------------------------------
#

#nsf_g16 <- read_csv("data/prd/USAspending_Exploration/NSF/FY16/All_Assistance_PrimeTransactions_2020-10-23_H19M21S10_1.csv")
#nsf_g17 <- read_csv("data/prd/USAspending_Exploration/NSF/FY17/All_Assistance_PrimeTransactions_2020-09-25_H17M36S08_1.csv")
nsf_g18 <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Assistance_PrimeTransactions_2020-09-25_H18M10S45_1.csv")


#
# BTM MODEL: code based on https://www.r-bloggers.com/2020/04/biterm-topic-modelling-for-short-texts/ -----------------------
#

# lowercase titles
titles <- tolower(nsf_g18$award_description)

# create model input: tokenize, lemmatize, POS.  Then create list of biterms with cooccurrence counts.

anno <- udpipe(titles, "english", trace = 10)
biterms <- as.data.table(anno)
biterms <- biterms[, cooccurrence(x = lemma,
                                  relevant = upos %in% c("NOUN", "ADJ", "VERB") & 
                                  nchar(lemma) > 2 & !lemma %in% stopwords("en"),
                                  skipgram = 3),
                  by = list(doc_id)]

# create model

set.seed(0)
traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) > 2)
traindata <- traindata[, c("doc_id", "lemma")]
model <- BTM(traindata, biterms = biterms, k = 20, iter = 2000, background = TRUE, 
             trace = 100, detailed = TRUE)

# output model results

plot(model, top_n = 10,
     title = "BTM model", subtitle = "NSF FY18 Grants",
     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"))


#
# MORE MODEL RESULTS: reference - https://cran.r-project.org/web/packages/BTM/BTM.pdf -----------------------
#

model

# top terms per topic
results <- terms(model, type = "tokens", threshold = 0, top_n = 10)

for(i in 1:20)
{
  cat("\n")
  print(paste0("Topic ", i))
  print(results[[i]])
}

# topic distribution per document

doc_topic_dist <- predict(model, newdata = traindata)
nrow(doc_topic_dist)
ncol(doc_topic_dist)
doc_topic_dist

# use LDAvis to visualize results  

# NOTE: if you want to use LDAvis, need to run BTM with detailed = TRUE so that the vocabulary 
# element of the model is created

docsize <- table(traindata$doc_id)
scores <- doc_topic_dist
scores <- scores[names(docsize), ]
json <- createJSON(
  phi = t(model$phi),
  theta = scores,
  doc.length = as.integer(docsize),
  vocab = model$vocabulary$token,
  term.frequency = model$vocabulary$freq)
serVis(json)
