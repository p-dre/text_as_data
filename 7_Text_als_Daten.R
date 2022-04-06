library(topicmodels)
library(dplyr)
library(tidytext)
library(udpipe)
library(quanteda)
library(lda)
library(ldatuning)
library(wordcloud)
library(doc2vec)
library(uwot)
library(tidyverse)

library("FactoMineR")
library(factoextra)










friends_data <- friends::friends

friends_scene <- friends_data %>% group_by(scene, episode, season) %>% summarise(full_scene = paste0(text, collapse = ""))  %>% arrange(season, episode, scene)


friends_data$doc_id <- gsub(" ", paste0(friends_data$speaker, friends_data$season, friends_data$episode, friends_data$scene, friends_data$utterance, '_' ,1:nrow(friends_data)),replacement =  "")


friends_corpus <- friends_data %>% corpus()

lemma_en <- udpipe_download_model(language = "english")
lemma_en <- udpipe_load_model(file = lemma_en$file_model)
lemma_en  <- udpipe(friends_corpus, lemma_en, parallel.cores = 8)
lemma_en <- lemma_en %>% filter(upos != 'PUNCT' & is.na(lemma) == F)

friends_token <-friends_corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english")) ) %>%
  tokens_ngrams( n = 1) 


friends_token <-   tokens_replace(tokens(friends_token), pattern = lemma_en$token, replacement = lemma_en$lemma)

friends_data_clean<- lapply(friends_token, function(x) paste(as.character(x), collapse = " "))

friends_data_clean <- data.frame( names(friends_data_clean) ,do.call(rbind.data.frame,friends_data_clean) )
names(friends_data_clean) <- c("doc_id", "text")



model <- paragraph2vec(x = friends_data_clean, type = "PV-DBOW", dim = 300, iter = 50)
embedding <- as.matrix(model)
friends_2d <- umap(embedding, n_neighbors = 15, n_threads = 4)
friends_2d <- as.data.frame(friends_2d)
friends_2d <-tibble::rownames_to_column(friends_2d)
names(friends_2d)<- c("doc_id","x1", "x2")
friends_data <- friends_data %>% left_join(friends_2d, friends_data, by= c("doc_id"= "doc_id"))
data <- friends_data%>%
  filter(speaker == "Monica Geller" | speaker == "Joey Tribbiani" | speaker == "Chandler Bing" | speaker == "Phoebe Buffay"
         | speaker =="Ross Geller" | speaker =="Rachel Green") %>%  column_to_rownames(var="doc_id") %>% select(x1,x2)




set.seed(123)
km.res <- kmeans(scale(data), 4)






# 3. Visualize

fviz_cluster(km.res, data = data,
              palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)




wss <- function(k) {
  kmeans( scale(data) , k,   iter.max=100 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- seq(2,100, 1)

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)




plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





km.res <- kmeans(scale(data), 60,   iter.max=100)


# 3. Visualize

fviz_cluster(km.res, data = data,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot", show.clust.cent = T,  geom = "points"
) 




library(lsa)


cosine_all <- function(k_model, data, cluster){
  
  
  cos <- cosine(as.vector(t(k_model$centers[cluster,])),as.vector(t(data)))
  
  
  
  return(cos)
  
}







list_top <- list()
for ( i in 1: nrow(km.res$centers) ){
  result <- data.frame(cos =  apply(X = data,1,FUN = cosine_all, k_model = km.res, cluster=i))
  list_top[[i]] <- result %>% arrange(cos) %>% top_n(50) %>% rownames_to_column("doc_id") %>% mutate(cluster= i)
  
}

top_word <- list()
for (list in 1:length(list_top)){
  
  top_word[[list]] <- list_top[[list]]  %>% 
    left_join(friends_data_clean %>% 
                select(doc_id, text), by= c("doc_id"= "doc_id"))%>%
    corpus() %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
    dfm(verbose = FALSE) 
  
}


top_word[[30]]%>% quanteda.textplots::textplot_wordcloud(comparison = F, max_words = 400,min_count = 0 , 
                                                        color = rev(RColorBrewer::brewer.pal(10, "RdBu")))






friends_data <- friends::friends
friends_corpus <- friends_data %>% corpus()

lemma_en <- udpipe_download_model(language = "english")
lemma_en <- udpipe_load_model(file = lemma_en$file_model)
lemma_en  <- udpipe(friends_corpus, lemma_en, parallel.cores = 8)
lemma_en <- lemma_en %>% filter(upos != 'PUNCT' & is.na(lemma) == F)

friends_token <-friends_corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english")) ) %>%
  tokens_ngrams( n = 1) 


friends_token <-   tokens_replace(tokens(friends_token), pattern = lemma_en$token, replacement = lemma_en$lemma)




friends_lda <- friends_token  %>%
                    dfm(verbose = F) %>%
                    convert( to = "topicmodels")



result_tuning <- ldatuning::FindTopicsNumber(
  friends_lda,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  topics = seq(from = 100, to = 200, by = 10),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result_tuning)


final_lda <- topicmodels::LDA(friends_lda, k = 140, method = "Gibbs")


terms(final_lda, 10)


Result <- posterior(final_lda)


top40 <- terms(final_lda , 40)
Result[1]

topic <- 10

top40terms <- sort(Result$terms[topic,], decreasing=TRUE)[1:100]
words <- names(top40terms)

probabilities <- sort(Result$terms[topic,], decreasing=TRUE)[1:100]

mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)





















