
#####################################################################################
#####################################################################################
##  The computer analysis of Latin texts:                                          ##
##  Gregorius_Turonensis_Topic_Modeling.                                           ##
##  Author: Alexey Kuznetsov                                                       ##
##  URL: https://github.com/alexeyvkuznetsov/Gregorius_Turonensis_Topic_Modeling   ##
##       https://alexeyvkuznetsov.github.io                                        ##
#####################################################################################
#####################################################################################



setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

library(textmineR)
library(igraph)
library(ggraph)
library(ggplot2)
library(tm)
library(udpipe)

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)
library(compiler)

t00<-paste(scan(file ="files/00.txt",what='character'),collapse=" ")
t01<-paste(scan(file ="files/01.txt",what='character'),collapse=" ")
t02<-paste(scan(file ="files/02.txt",what='character'),collapse=" ")
t03<-paste(scan(file ="files/03.txt",what='character'),collapse=" ")
t04<-paste(scan(file ="files/04.txt",what='character'),collapse=" ")
t05<-paste(scan(file ="files/05.txt",what='character'),collapse=" ")
t06<-paste(scan(file ="files/06.txt",what='character'),collapse=" ")
t07<-paste(scan(file ="files/07.txt",what='character'),collapse=" ")
t08<-paste(scan(file ="files/08.txt",what='character'),collapse=" ")
t09<-paste(scan(file ="files/09.txt",what='character'),collapse=" ")
t10<-paste(scan(file ="files/10.txt",what='character'),collapse=" ")


t00<-data.frame(texts=t00)
t01<-data.frame(texts=t01)
t02<-data.frame(texts=t02)
t03<-data.frame(texts=t03)
t04<-data.frame(texts=t04)
t05<-data.frame(texts=t05)
t06<-data.frame(texts=t06)
t07<-data.frame(texts=t07)
t08<-data.frame(texts=t08)
t09<-data.frame(texts=t09)
t10<-data.frame(texts=t10)


t00$book<-"Praefatio"
t01$book<-"Liber 01"
t02$book<-"Liber 02"
t03$book<-"Liber 03"
t04$book<-"Liber 04"
t05$book<-"Liber 05"
t06$book<-"Liber 06"
t07$book<-"Liber 07"
t08$book<-"Liber 08"
t09$book<-"Liber 09"
t10$book<-"Liber 10"


historia<-rbind(t00,t01,t02,t03,t04,t05,t06,t07,t08,t09,t10)

historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)


#########################################################################
### REMOVE STOPWORDS                                                  ###
#########################################################################

#rome_number<-paste(scan(file ="rom number 1000.txt",what='character'),collapse=" ")
#rome_number<-tolower(rome_number)
#rome_number
#write(rome_number, file="rome_number_v.txt")



load("rome_number_1000.Rda")

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")

lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut", "quoque", "xiix")

#save(lat_stop_perseus,file="lat_stop_perseus.Rda")

#load("lat_stop_perseus.Rda")

#MyStopwords <- c(lat_stop_perseus, customStopWords, lat_stopwords_romnum)

MyStopwords <- c(lat_stop_perseus, rome_number_1000, customStopWords)

#historia$texts <- removeWords(historia$texts, c(lat_stop_perseus, rome_number_1000))

historia$texts <- removeWords(historia$texts, MyStopwords)

historia$texts <- stripWhitespace(historia$texts)


#########################################################################
### UDPipe ANNOTATION                                                 ###
### http://ufal.mff.cuni.cz/udpipe/models/                            ###
#########################################################################


# UDPipe annotation
#udmodel_latin <- udpipe_download_model(language = "latin-ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.5-191206.udpipe")


x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)


save(x,file="hustoria_annotated_dataset.Rda")


#load("fivebooks_annotated_dataset.Rda")



dtf <- subset(x, upos %in% c("NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

head(dtf)


dtm <- document_term_matrix(x = dtf)

dtm <- dtm_remove_lowfreq(dtm, minfreq = 4)

head(dtm_colsums(dtm))

dtm <- dtm_remove_terms(dtm, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))



# Coherence score for topics

k_list <- seq(1,15, by=1)

model_dir <- paste0("models_", digest::digest(colnames(dtm), algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)


model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  m <- FitLdaModel(dtm = dtm, 
                   k = k, 
                   iterations = 4000, 
                   burnin = 500,
                   alpha = 0.1,
                   optimize_alpha = FALSE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= c("dtm"), 
cpus = 2)


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Оптимальное количество тем (k)") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,15,1)) + ylab("Когерентность тем")




# Topic modeling

library(topicmodels)

topicModel <- topicmodels::LDA(dtm, k = 4, method = "Gibbs", control = list(nstart = 5, iter = 4000, burnin = 500, best = TRUE, seed = 1:5, alpha = 0.1))

topics(topicModel)


# Topics vizualization

library(tidytext)
library(ggplot2)
library(dplyr)


td_beta <- tidy(topicModel)
td_beta %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Наиболее часто встречающиеся слова для каждой темы")






### Topic proportions https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html


textIds <- c(1, 2, 3, 4, 5)

lapply(fivebooks$texts[textIds], as.character)

tmResult <- posterior(topicModel)

theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel, 7), 2, paste, collapse = " ")  

attr(topicModel, "alpha")

# load libraries for visualization

library("reshape2")
library("ggplot2")


# get topic proportions form example documents

N <- 5

topicProportionExamples <- theta[textIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)







