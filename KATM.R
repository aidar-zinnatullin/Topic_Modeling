# Time index  
library(lubridate)
library(here)
library(tidyverse)
library(keyATM)
library(rlang)
library(quanteda)
library(quanteda.corpora)
# Convert date into integer 


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



guardian_clean_text <- texts_tokens %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " "))

guardian_clean_text$data <- NULL
names(guardian_clean_text)


key_corpus <- corpus(guardian_clean_text, text_field = "text")

# You can conduct a variety of preprocessing in this step as shown in the next section
key_token <- tokens(key_corpus) %>%
  tokens_select(min_nchar = 3)

# Create a document-feature matrix (a dfm object) from a token object
key_dfm <- dfm(key_token)  %>%
  dfm_trim(min_termfreq = 2, min_docfreq = 2)

ncol(key_dfm)  # the number of unique words
keyATM_docs <- keyATM_read(texts = key_dfm)
summary(keyATM_docs)



keywords <- list(
  US     = c("trump", "sanders", "clinton"),
  Russia       = c("russia", "putin"),
  Economy          = c("recession", "tax", "business")
)




index <- as.integer(gsub("-", "", format(docvars(key_corpus)$date, "%Y-%m")))

# Replace elements

char_index <- as.character(index)

# Condition 
given <- sort(unique(index)) %>% as.character()

# For loop 
for (i in seq(1:length(unique(index)))){
  
  char_index[char_index == given[i]] = paste(i)
  
  message(paste("replaced", i))
  
}

# Check 
unique(char_index) %>% as.numeric() %>% sort()

# Arrange and assign 
docvars(key_corpus, "index") <- char_index %>% as.integer()

docvars(key_corpus) <- docvars(key_corpus) %>%
  arrange(index)
vars <- docvars(key_corpus)

out_dynamic <- keyATM(
  docs= keyATM_docs,
  no_keyword_topics = 10,
  keywords          = keywords,
  model             = "dynamic",
  model_settings    = list(time_index = vars$index,  num_states = 3),
  options           = list(seed = 250, store_theta = TRUE, thinning =3)
)
rlang::inject(qs::qsavem(out_dynamic, file = "dynamicTopicModel10.qs"))

qs::qload("dynamicTopicModel10.qs")

top_words(out_dynamic)

vars$date <- as.Date(vars$date)

# function to round dates to the nearest six-month period
round_date_to_half_year <- function(date) {
  month <- month(date)
  half_year_start_month <- 6 * (floor((month - 1) / 6)) + 1
  rounded_date <- as.Date(paste(year(date), half_year_start_month, "01", sep = "-"))
  return(rounded_date)
}

vars$date_rounded <- sapply(vars$date, round_date_to_half_year)

vars$date_rounded <- as.Date(vars$date_rounded, origin = "1970-01-01")

time_index_label <- format(vars$date_rounded, "%Y-%m")

fig_timetrend <- plot_timetrend(out_dynamic, time_index_label = time_index_label, xlab = "Time")
fig_timetrend
