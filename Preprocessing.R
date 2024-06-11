# Details:
# Denny, M. J., & Spirling, A. (2018). Text Preprocessing For Unsupervised Learning: Why It Matters, When It Misleads, And What To Do About It. Political Analysis, 26(2), 168–189. doi:10.1017/pan.2017.44
# http://www.mjdenny.com/getting_started_with_preText.html

library(here)
library(quanteda)
library(quanteda.corpora)
library(stringr)
library(tidyverse)
library(tidytext)
library(stm)
library(geometry)
library(Rtsne)
library(rsvd)
library(SnowballC)
library(plotrix)

#devtools::install_github("matthewjdenny/preText")
library(preText)

# download the corpus

corp <- corpus(download('data_corpus_guardian'))

documents <- corp[1:1000,]

preprocessed_documents <- factorial_preprocessing(
  documents,
  use_ngrams = TRUE,
  infrequent_term_threshold = 0.2,
  verbose = FALSE)

names(preprocessed_documents)

head(preprocessed_documents$choices)

preText_results <- preText(
  preprocessed_documents,
  dataset_name = "Guardian",
  distance_method = "cosine",
  num_comparisons = 20,
  verbose = FALSE)

# the least risky specifications have the lowest preText score and are displayed at the top of the plot
jpeg("Figures/FigureCheckPreproc.jpeg", width = 16, height = 14, units = 'in', res = 500)
preText_score_plot(preText_results)
dev.off()

jpeg("Figures/FigureConditionalPreprocess.jpeg", width = 10, height = 8, units = 'in', res = 500)
regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)
dev.off()





# Download the Guardian Corpus from Quanteda ------------------------------

corp <- corpus(download('data_corpus_guardian'))

df_guardian <- convert(corp, to = "data.frame")  


# Start STM part of the analysis ------------------------------------------


processed <- textProcessor(df_guardian$text, metadata = df_guardian, lowercase = FALSE, removestopwords = FALSE, 
                           removepunctuation = TRUE, removenumbers = TRUE, stem = TRUE) 

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100)) 

out <- prepDocuments(processed$documents, processed$vocab,processed$meta) 
docs <- out$documents
vocab <- out$vocab
meta <-out$meta



out$meta$date <- as.numeric(as.Date(out$meta$date)) # It is necessary to avoid the cdata problem when we include  to the model the time variable as a covariate


modelPrevFit_0_2 <- stm(documents = out$documents, vocab = out$vocab,
                      K = 0, prevalence = ~ s(date),
                      max.em.its = 100,
                      data = out$meta,
                      init.type = "Spectral",
                      seed = 123) 
save(modelPrevFit_0_2, file = "stm_outputs/ModelDennySpurling.RData")
load("stm_outputs/PreliminaryModel.RData")

labelTopics(modelPrevFit_0, n = 10) # 70
labelTopics(modelPrevFit_0_2, n = 10) # 57


jpeg("Figures/FigurePreprocess.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(modelPrevFit_0_2, type = "summary", xlim = c(0, .13), n = 10)
dev.off()

jpeg("Figures/Figure70K.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(modelPrevFit_0, type = "summary", xlim = c(0, .13), n = 10)
dev.off()



