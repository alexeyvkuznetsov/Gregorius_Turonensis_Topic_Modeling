# text2vec
# http://text2vec.org/topic_modeling.html#topic=0&lambda=0.5&term=

setwd("D:/GitHub/Gregorius_Turonensis_Topic_Modeling/")

library(stringr)
library(text2vec)
data("movie_review")
# select 1000 rows for faster running times
movie_review_train = movie_review[1:700, ]
movie_review_test = movie_review[701:1000, ]
prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alpha:]]", " ")
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
}
movie_review_train$review = prep_fun(movie_review_train$review)
it = itoken(movie_review_train$review, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)

tfidf = TfIdf$new()
lsa = LSA$new(n_topics = 10)

# pipe friendly transformation
doc_embeddings =  fit_transform(dtm, tfidf)
doc_embeddings =  fit_transform(doc_embeddings, lsa)



