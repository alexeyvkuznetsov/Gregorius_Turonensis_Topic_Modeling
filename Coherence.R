library(textmineR)

# https://github.com/clawilso15/Text.Replace/blob/master/inst/in-work/topicModelling.R

# до этого момента создаем dtm

ntopics <- 40
model_list <- FitLdaModel(dtm = dtm, 
                              k = ntopics, 
                              iterations = 1000,
                              burnin = 500,
                              alpha = 0.1,
                              optimize_alpha = FALSE,
                              calc_likelihood = FALSE,
                              calc_coherence = TRUE,
                              calc_r2 = FALSE,
                              cpus = 1)
model_list$k <- ntopics
model_list$coherence <- CalcProbCoherence(phi = model_list$phi, 
                                          dtm, M = 5)



coherence_mat <- data.frame(k = 1:ntopics, 
                            coherence = model_list$coherence, 
                            stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x=k, y=coherence))+
  geom_point()+
  geom_line(group = 1) + 
  ggtitle("Best Topic by Coherence Score") + 
  theme_minimal() + 
  scale_x_continuous()+
  ylab("Coherence")
