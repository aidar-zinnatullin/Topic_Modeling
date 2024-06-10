
# Install required rackages -----------------------------------------------

install.packages(here)
install.packages(quanteda)
install.packages(devtools)
devtools::install_github("quanteda/quanteda.corpora")
install.packages(quanteda.corpora)
install.packages(stringr)
install.packages(tidyverse)
install.packages(tidytext)
install.packages(stm)
install.packages(geometry)
install.packages(Rtsne)
install.packages(rsvd)
install.packages(SnowballC)
install.packages(plotrix)


# Load the packages -------------------------------------------------------


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


# Download the Guardian Corpus from Quanteda ------------------------------

corp <- corpus(download('data_corpus_guardian'))

df_guardian <- convert(corp, to = "data.frame")  

names(df_guardian)

df_guardian$text <- str_trim(df_guardian$text, side = "both")



# Start preprocessing -----------------------------------------------------


df_guardian$text <- tolower(df_guardian$text)
df_guardian$text <- gsub("[[:punct:]]", " ", df_guardian$text)

df_guardian$text <- gsub("^[[:space:]]*", "", df_guardian$text) 
df_guardian$text <- gsub("[[:space:]]*$", "", df_guardian$text) 
df_guardian$text <- gsub("[0-9]+", "", df_guardian$text)
df_guardian$text <- gsub("\\s+", " ", df_guardian$text)


# Remove stopwords
new_stopwords <- read.table("stopwords.txt") # 736 words
names(new_stopwords) <- "word"

new_stopwords <- as_tibble(new_stopwords)

texts_tokens <- df_guardian %>%
  unnest_tokens(word, text)


texts_tokens <- texts_tokens %>%
  anti_join(new_stopwords)


texts_tokens <- texts_tokens %>%
  mutate(word = wordStem(word))


head(texts_tokens)
guardian_clean_text <- texts_tokens %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " "))

table(df_guardian$doc_id%in% guardian_clean_text$doc_id) # missing four articles
removed_df <- df_guardian[df_guardian$doc_id%in% guardian_clean_text$doc_id==FALSE,] # text column is empty


head(guardian_clean_text)
str(guardian_clean_text)
guardian_clean_text$data <- NULL


# Start STM part of the analysis ------------------------------------------


processed <- textProcessor(guardian_clean_text$text, metadata = guardian_clean_text) 

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100)) 

out <- prepDocuments(processed$documents, processed$vocab,processed$meta, lower.thresh = 1) # decided to deal with threshold 1
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
save(out, docs, vocab, meta, processed, guardian_clean_text, texts_tokens, df_guardian, new_stopwords, 
     file = "stm_outputs/saved_objects.RData")



# selecting number of topic -----------------------------------------------

# I present two options: (1) the method proposed by Lee & Mimno, 2014;
# (2) an iterative approach based on the elbow principle from the cluster analysis, estimating the trade-off between semantic coherence and exclusivity



#  Option 1: an automated method to select the number of topics (Lee & Mimno, 2014)
# k = 0 and use the Spectral initialization
# "We emphasize that this procedure has no particular statistical guarantees and should not
# be seen as estimating the “true” number of topics. However it can be useful place to start and
# has the computational advantage that it only needs to be run once." (Roberts, Stewart, and Tingley, 2019, p. 13)

#load(here("stm_outputs","saved_objects.RData"))

out$meta$date <- as.numeric(as.Date(out$meta$date)) # It is necessary to avoid the cdata problem when we include  to the model the time variable as a covariate


modelPrevFit_0 <- stm(documents = out$documents, vocab = out$vocab,
                          K = 0, prevalence = ~ s(date),
                          max.em.its = 100,
                          data = out$meta,
                          init.type = "Spectral",
                          seed = 123) # 20:48 - 21:40, so it can take around an hour on CPU
save(modelPrevFit_0, file = "stm_outputs/PreliminaryModel.RData")

#load(here("stm_outputs", "PreliminaryModel.RData"))

labelTopics(modelPrevFit_0, n = 10) # 70


options(scipen = 999)
prep <- estimateEffect(1:70 ~  s(date), modelPrevFit_0, meta = out$meta, uncertainty = "Global") 
summary(prep, topics=1)
plot(modelPrevFit_0, type = "summary", xlim = c(0, .13))
plot(prep, "date", method = "continuous", topics = 63, model = modelPrevFit_0, printlegend = FALSE, xaxt = "n", xlab = "")
save(prep, file = "stm_outputs/prep_mimno_tsur.RData")


# Option 2: Semantic Coherence - Exclusivity trade-off
storage_10_100 <- searchK(out$documents, out$vocab, K = seq(from = 10, to = 100, by = 5),
                             prevalence =~ s(date), data = meta) # it will take a while
storage_10_100$results
storage_10_100_k <- storage_10_100$results
str(storage_10_100_k)
storage_10_100_k$K <- unlist(storage_10_100_k$K)
storage_10_100_k$K
storage_10_100_k$exclus <- unlist(storage_10_100_k$exclus)
storage_10_100_k$semcoh <- unlist(storage_10_100_k$semcoh)
storage_10_100_k$heldout <- unlist(storage_10_100_k$heldout)
storage_10_100_k$residual <- unlist(storage_10_100_k$residual)
storage_10_100_k$bound <- unlist(storage_10_100_k$bound)
storage_10_100_k$lbound <- unlist(storage_10_100_k$lbound)
storage_10_100_k$em.its <- unlist(storage_10_100_k$em.its)  
str(storage_10_100_k)

write.csv(x = storage_10_100_k, file = "stm_outputs/semantic_coherence_1.csv")

topics_metrics <-storage_10_100_k[,1:3]


# Visualize the trade-off between semantic coherence and exclusivity, first iteration

jpeg("Figures/Figure1.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(x = topics_metrics$semcoh, y = topics_metrics$exclus,type = "p", ylab = "Exclusivity", 
     xlab = "Semantic Coherence")
text(topics_metrics$exclus~topics_metrics$semcoh, labels=topics_metrics$K, data=topics_metrics, cex=0.8, font=2.9)
xl <- seq(min(topics_metrics$semcoh),max(topics_metrics$semcoh), (max(topics_metrics$semcoh) - min(topics_metrics$semcoh))/1000)
lo <- loess(topics_metrics$exclus~topics_metrics$semcoh)
lines(xl, predict(lo,xl), col='red', lwd=2)
#draw.circle(x=-110, y=9.75, radius=4.5,border = "red")
text(x =-70, y = 9.7, labels = "K-Values for Next Range (K = 15-35)", col = "blue")
dev.off()


# Need to run a new round of K serching ----------------------------------

storage_15_35 <- searchK(out$documents, out$vocab, K = c(15:35),
                               prevalence =~ s(date), data = meta) 

storage_15_35$results
storage_15_35_k <- storage_15_35$results
str(storage_15_35_k)
storage_15_35_k$K <- unlist(storage_15_35_k$K)
storage_15_35_k$K
storage_15_35_k$exclus <- unlist(storage_15_35_k$exclus)
storage_15_35_k$semcoh <- unlist(storage_15_35_k$semcoh)
storage_15_35_k$heldout <- unlist(storage_15_35_k$heldout)
storage_15_35_k$residual <- unlist(storage_15_35_k$residual)
storage_15_35_k$bound <- unlist(storage_15_35_k$bound)
storage_15_35_k$lbound <- unlist(storage_15_35_k$lbound)
storage_15_35_k$em.its <- unlist(storage_15_35_k$em.its)  
str(storage_15_35_k)



write.csv(x = storage_15_35_k, file = "stm_outputs/semantic_coherence_2.csv")

topics_metrics_2 <-storage_15_35_k[,1:3]



jpeg("Figures/Figure2.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(x = topics_metrics_2$semcoh, y = topics_metrics_2$exclus,type = "p", ylab = "Exclusivity", 
     xlab = "Semantic Coherence")
text(topics_metrics_2$exclus~topics_metrics_2$semcoh, labels=topics_metrics_2$K, data=topics_metrics_2, cex=0.8, font=2.9)
#draw.circle(x=-99.8, y=9.61, radius=1.9,border = "red")
xl <- seq(min(topics_metrics_2$semcoh),max(topics_metrics_2$semcoh), (max(topics_metrics_2$semcoh) - min(topics_metrics_2$semcoh))/1000)
lo <- loess(topics_metrics_2$exclus~topics_metrics_2$semcoh)
lines(xl, predict(lo,xl), col='red', lwd=2)
text(x =-69, y = 9.70, labels = "K-Values for Next Range (K=20-30)", col = "blue")
dev.off()



# -------------------------------------------------------------------------

storage_20_30 <- searchK(out$documents, out$vocab, K = c(20:30),
                               prevalence =~ s(date), data = meta) 

storage_20_30$results
storage_20_30_k <- storage_20_30$results
str(storage_20_30_k)
storage_20_30_k$K <- unlist(storage_20_30_k$K)
storage_20_30_k$K
storage_20_30_k$exclus <- unlist(storage_20_30_k$exclus)
storage_20_30_k$semcoh <- unlist(storage_20_30_k$semcoh)
storage_20_30_k$heldout <- unlist(storage_20_30_k$heldout)
storage_20_30_k$residual <- unlist(storage_20_30_k$residual)
storage_20_30_k$bound <- unlist(storage_20_30_k$bound)
storage_20_30_k$lbound <- unlist(storage_20_30_k$lbound)
storage_20_30_k$em.its <- unlist(storage_20_30_k$em.its)
str(storage_20_30_k)

write.csv(x = storage_20_30_k, file = "stm_outputs/semantic_coherence_3.csv")

topics_metrics_3 <-storage_20_30_k[,1:3]



jpeg("Figures/Figure3.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(x = topics_metrics_3$semcoh, y = topics_metrics_3$exclus,type = "p", ylab = "Exclusivity", 
     xlab = "Semantic Coherence")
text(topics_metrics_3$exclus~topics_metrics_3$semcoh, labels=topics_metrics_3$K, data=topics_metrics_3, cex=0.8, font=2.9)
xl <- seq(min(topics_metrics_3$semcoh),max(topics_metrics_3$semcoh), (max(topics_metrics_3$semcoh) - min(topics_metrics_3$semcoh))/1000)
lo <- loess(topics_metrics_3$exclus~topics_metrics_3$semcoh)
lines(xl, predict(lo,xl), col='red', lwd=2)
draw.circle(x=-70.30011, y=9.673961, radius=0.2,border = "red")
text(x =-70.30011, y = 9.69, labels = "Final K-Value (K=27)", col = "blue")
dev.off()


# Finally, we run STM with k = 27

modelPrevFit_27 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 27, prevalence = ~  s(date), 
                       max.em.its = 1000, data = out$meta,
                       init.type = "Spectral", seed = 12345)

save(modelPrevFit_27, file = "stm_outputs/modelPrevFit_27.RData")
#load(here("stm_outputs", "modelPrevFit_27.RData"))
labelTopics(modelPrevFit_27)


# Topics, 27 --------------------------------------------------------------

jpeg("Figures/Vis_27.jpeg", width = 9, height = 6, units = 'in', res = 500)
plot(modelPrevFit_27, type = "summary", xlim = c(0, .13))
dev.off()


# Shares of topic
# -------------------------------------------------------------------------


theta <- modelPrevFit_27$theta
average_topic_proportions <- colMeans(theta)
print(average_topic_proportions)
options(scipen = 999)

# Dynamic Topics ----------------------------------------------------------

reg_model_2 <- estimateEffect(1:27 ~  s(date), modelPrevFit_27, meta = out$meta, uncertainty = "Global") 
summary(reg_model_2, topics=1)
save(reg_model_2, file = "stm_outputs/STM_27.RData")


start_date <- as.Date("2012-01-02")
end_date <- as.Date("2016-12-31")

date_seq <- seq(from = start_date, to = end_date, by = "100 days")

date_seq_numeric <- as.numeric(date_seq)

date_labels <- format(date_seq, "%Y-%m")

jpeg("Figures/Vis_Dynamic.jpeg", width = 9, height = 6, units = 'in', res = 500)
plot(reg_model_2, "date", method = "continuous", topics = 27, model = modelPrevFit_27, 
     printlegend = FALSE, xaxt = "n", xlab = "")
axis(1, at = date_seq_numeric, labels = date_labels, las = 2)
mtext("Time", side = 1, line = 4)
dev.off()



# Interpreting the topics -------------------------------------------------
labelTopics(modelPrevFit_27)
dt_texts <- make.dt(modelPrevFit_27, meta=meta)
dt_texts <- as.data.frame(dt_texts)
names(dt_texts)
#for topic 1 to present a better example: Natural Disasters
labelTopics(modelPrevFit_27)
dt_texts[which.max(dt_texts$Topic1), c('text', 'Topic1')] 
dt_texts %>% arrange(desc(Topic1) ) %>% select(text, Topic1) %>% top_n(n = 15)

# topic 2 - UK Party Politics
labelTopics(modelPrevFit_27)
dt_texts[which.max(dt_texts$Topic2), c('text', 'Topic2')] 
dt_texts %>% arrange(desc(Topic2)) %>%dplyr::select(text, Topic2) %>% top_n(n = 35) 


# topic 3 - Curation, Preservation, and Presentation of Historical and Artistic Artifacts
labelTopics(modelPrevFit_27)
dt_texts[which.max(dt_texts$Topic3), c('text', 'Topic3')]
dt_texts %>% arrange(desc(Topic3)) %>% select(text, Topic3) %>% top_n(n = 15) 

# topic 4 - Culture
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic4), c('text', 'Topic4')]
dt_texts %>% arrange(desc(Topic4)) %>% dplyr::select(text, Topic4) %>% top_n(n = 15) 


# topic 5 - Financial System
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic5), c('text', 'Topic5')]
dt_texts %>% arrange(desc(Topic5)) %>% select(text, Topic5) %>% top_n(n = 35) 

# topic 6 - Asia
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic6), c('text', 'Topic6')]
dt_texts %>% arrange(desc(Topic6)) %>% select(text, Topic6) %>% top_n(n = 25) 

# topic 7 - Gun Control
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic7), c('text', 'Topic7')]
dt_texts %>% arrange(desc(Topic7)) %>% select(text, Topic7) %>% top_n(n = 25) 

# topic 8 - UK Parliament
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic8), c('text', 'Topic8')]
dt_texts %>% arrange(desc(Topic8)) %>% select(text, Topic8) %>% top_n(n = 25) 

# topic 9 - Infrastructure and Public Safety
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic9), c('text', 'Topic9')]
dt_texts %>% dplyr::select(text, Topic9) %>% arrange(desc(Topic9)) %>%  top_n(n = 15) 


# topic 10 - Russia
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic10), c('text', 'Topic10')]
dt_texts %>% arrange(desc(Topic10)) %>% select(text, Topic10) %>% top_n(n = 15) 

# topic 11 - Refugee Crisis
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic11), c('text', 'Topic11')]
dt_texts %>% arrange(desc(Topic11)) %>% select(text, Topic11) %>% top_n(n = 25) 

# topic 12 - US Elections
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic12), c('text', 'Topic12')]
dt_texts %>% arrange(desc(Topic12)) %>% dplyr::select(text, Topic12) %>% top_n(n = 25) 

# topic 13 - Sustainable Development
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic13), c('text', 'Topic13')]
dt_texts %>% arrange(desc(Topic13)) %>% select(text, Topic13) %>% top_n(n = 25) 

# topic 14 - Energy
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic14), c('text', 'Topic14')]
dt_texts %>% arrange(desc(Topic14)) %>% select(text, Topic14) %>% top_n(n = 25) 

# topic 15 - Housing Policy
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic15), c('text', 'Topic15')]
dt_texts %>% arrange(desc(Topic15)) %>% select(text, Topic15) %>% top_n(n = 25) 

# topic 16 - Health
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic16), c('text', 'Topic16')]
dt_texts %>% arrange(desc(Topic16)) %>% select(text, Topic16) %>% top_n(n = 25) 

# topic 17 - Personal Stories (Career, Challenges, etc.)
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic17), c('text', 'Topic17')]
dt_texts %>% arrange(desc(Topic17)) %>% select(text, Topic17) %>% top_n(n = 25) 

# topic 18 - Technology
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic18), c('text', 'Topic18')]
dt_texts %>% arrange(desc(Topic18)) %>% select(text, Topic18) %>% top_n(n = 25) 

# topic 19 - Environmental Changes
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic19), c('text', 'Topic19')]
dt_texts %>% arrange(desc(Topic19)) %>% select(text, Topic19) %>% top_n(n = 25) 

# topic 20 - Family Issues
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic20), c('text', 'Topic20')]
dt_texts %>% arrange(desc(Topic20)) %>% select(text, Topic20) %>% top_n(n = 25) 

# topic 21 - Social Media
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic21), c('text', 'Topic21')]
dt_texts %>% arrange(desc(Topic21)) %>% select(text, Topic21) %>% top_n(n = 25) 

# topic 22 - Islamism
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic22), c('text', 'Topic22')]
dt_texts %>% arrange(desc(Topic22)) %>% select(text, Topic22) %>% top_n(n = 25) 

# topic 23 - Court Hearings
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic23), c('text', 'Topic23')]
dt_texts %>% arrange(desc(Topic23)) %>% select(text, Topic23) %>% top_n(n = 25) 

# topic 24 - Live Broadcast
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic24), c('text', 'Topic24', 'head')]
dt_texts %>% arrange(desc(Topic24)) %>% select(text, Topic24) %>% top_n(n = 25) 

# topic 25 - Taxes, Banking
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic25), c('text', 'Topic25')]
dt_texts %>% arrange(desc(Topic25)) %>% select(text, Topic25) %>% top_n(n = 25) 

# topic 26 - Business News
labelTopics(modelPrevFit_27, n = 7)
dt_texts[which.max(dt_texts$Topic26), c('text', 'Topic26')]
dt_texts %>% arrange(desc(Topic26)) %>% select(text, Topic26) %>% top_n(n = 25) 

# topic 27 - Elections
dt_texts[which.max(dt_texts$Topic27), c('text', 'Topic27')]
dt_texts %>% arrange(desc(Topic27)) %>% select(text, Topic27) %>% top_n(n = 25) 


# labels and visualization ----------------------------------------------------------


topic_labels <- c("Natural Disasters", "UK Party Politics", "Presentation of Historical and Artistic Artifacts",
                  "Culture", "Financial System", "Asia", "Gun Control", "UK Parliament", "Infrastructure and Public Safety", 
                  "Russia", "Refugee Crisis", "US Elections", "Sustainable Development", "Energy", "Housing Policy", 
                  "Health", "Personal Stories (Career, Challenges, etc.)", "Technology", "Environmental Changes", 
                  "Family Issues", "Social Media", "Islamism", "Court Hearings", "Live Broadcast", "Taxes, Banking", 
                  "Business News", "Elections")


jpeg("Figures/VisTopics.jpeg", width = 12, height = 8, units = 'in', res = 500)
par(mar = c(4, 22,4,4))
barplot(average_topic_proportions, names.arg = topic_labels,
        main="Topic Proportions", horiz = TRUE, xlab="Proportion", col="steelblue", las = 2)
dev.off()
# -------------------------------------------------------------------------


