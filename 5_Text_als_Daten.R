

library(friends)
library(dplyr)
library(tidytext)
library(quanteda)
library(quanteda.textplots)
setwd('C:/Users/Drecker/Documents/Lehre')
load(".\\Daten\\harry_data.Rda")


harry_data %>% corpus %>%  tokens() %>%
  dfm(verbose = FALSE)

harry_data %>% 
  corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) 

harry_data %>% 
  corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english"))) %>%
  tokens_ngrams( n = 1)



harry_data %>%
  corpus %>%  
  tokens() %>%
  dfm(verbose = FALSE) %>%
  dfm_group(groups = title) %>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max.words = 300,title.size = 1)


harry_data %>%
  corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  dfm(verbose = FALSE) %>%
  dfm_group(groups = title) %>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max.words = 300,title.size = 1)


harry_data %>%
  corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english"))) %>%
  tokens_ngrams( n = 1) %>%
  dfm(verbose = FALSE) %>%
  dfm_group(groups = title) %>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max.words = 300,title.size = 1)






harry_data %>%
  corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, "c") %>% 
  tokens_remove( c(stopwords("english"))) %>%
  tokens_wordstem( language = quanteda_options("english")) %>%
  tokens_ngrams( n = 1) %>%
  dfm(verbose = FALSE) %>%
  dfm_group(groups = title) %>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max.words = 300,title.size = 1)








harry_corpus <- harry_data %>% corpus()

lemma_en <- udpipe_download_model(language = "english")
lemma_en <- udpipe_load_model(file = lemma_en$file_model)
lemma_en  <- udpipe(harry_corpus, lemma_en, parallel.cores = 8)
lemma_en <- lemma_en %>% filter(upos != 'PUNCT' & is.na(lemma) == F)

harry_token <-harry_corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english"),"c") ) %>%
  tokens_ngrams( n = 1) 


harry_token <-   tokens_replace(tokens(harry_token), pattern = lemma_en$token, replacement = lemma_en$lemma)


harry_token  %>%
  dfm(verbose = FALSE) %>%
  dfm_group(groups = title)

harry_dfm <- harry_token  %>%
              dfm(verbose = FALSE) %>%
              dfm_group(groups = title)

harry_dfm%>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max.words = 300,title.size = 1)


library(reshape2)

library(wordcloud)

 harry_token  %>%
  dfm(verbose = F)  %>% 
  dfm_group(groups = title) %>% 
  dfm_tfidf() %>%
  tidy()%>%
  acast(term ~ document, value.var = "count", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = FALSE, max.words = 300)
 