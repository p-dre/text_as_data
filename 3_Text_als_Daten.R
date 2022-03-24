library(friends)
library(dplyr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(word2vec)
library(harrypotter)
library(uwot)
library(plotly)
library(doc2vec)

dplyr::glimpse(friends)
friends_data <- friends::friends


friends_data %>%
  filter(speaker == "Monica Geller" | speaker == "Joey Tribbiani" | speaker == "Chandler Bing" | speaker == "Phoebe Buffay" | speaker == 'Rachel Green'
           | speaker =="Ross Geller") %>%
  group_by(speaker) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  arrange(desc(n), .by_group = T)  %>%
  acast(word ~ speaker, value.var = "n", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = FALSE, max.words = 500)





#devtools::install_github("bradleyboehmke/harrypotter")


titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")
books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

harry_data <- tibble()
for ( i in seq_along(books)){
  book <- tibble(title = titles[i],text = toString(books[[i]]))
  harry_data  <- rbind(harry_data, book)

}





harry_data %>% group_by(title) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  arrange(desc(n), .by_group = T)  %>%
  acast(word ~ title, value.var = "n", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = T,max.words=400,scale = c(2, 0.5))





# n-gramm

harry_data %>% group_by(title) %>%
  unnest_tokens(bigram,text,token = "ngrams", n = 2 ) %>%
  count(bigram) %>%
  arrange(desc(n), .by_group = T)  %>%
  acast(bigram ~ title, value.var = "n", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = T,max.words=200,scale = c(2, 0.5))




# tf–idf


harry_data %>% unnest_tokens(word, text) %>% group_by(title) %>%
  count(word) %>%
  bind_tf_idf(word, title, n) %>%
  acast(word ~ title, value.var = "tf_idf", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = T,max.words=200,scale = c(2, 0.5),)




# word2vec
set.seed(123456789)




vec_harry <- sapply(harry_data$text, paste, collapse=".")
model <- word2vec(x = vec_harry, type = "cbow", dim = 300, iter = 50, threads	= 4)
embedding <- as.matrix(model)
labels <- harry_data$title
harry_2d <- umap(embedding, n_neighbors = 15, n_threads = 4)
harry_2d <- as.data.frame(harry_2d)
names(harry_2d) <- c("x1", "x2")

fig <- plot_ly(harry_2d, x = ~x1, y = ~x2, type = 'scatter', mode = 'markers', text = rownames(embedding) ) %>%
  layout(plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))

fig
harry_2d %>% filter(x1<0 -4)




# Friends doc2vec
friends_scene <- friends_data %>% group_by(scene, episode, season) %>% summarise(full_scene = paste0(text, collapse = ""))  %>% arrange(season, episode, scene)


friends_data$doc_id <- gsub(" ", paste0(friends_data$speaker, friends_data$season, friends_data$episode, friends_data$scene, friends_data$utterance),replacement =  "")



model <- paragraph2vec(x = friends_data, type = "PV-DBOW", dim = 300, iter = 50)
embedding <- as.matrix(model)
friends_2d <- umap(embedding, n_neighbors = 15, n_threads = 4)
friends_2d <- as.data.frame(friends_2d)
friends_2d <-tibble::rownames_to_column(friends_2d)
names(friends_2d)<- c("doc_id","x1", "x2")
friends_data <- friends_data %>% inner_join(friends_2d, friends_data, by= c("doc_id"= "doc_id"))
data <- friends_data%>%
  filter(speaker == "Monica Geller" | speaker == "Joey Tribbiani" | speaker == "Chandler Bing" | speaker == "Phoebe Buffay"
         | speaker =="Ross Geller")




fig <- plot_ly(data , x = ~x1.y, y = ~x2.y, type = 'scatter', mode = 'markers', color = data$season, text = data$speaker) %>%
  layout(plot_bgcolor = "#e5ecf6",
         xaxis = list(
           title = "0"),
         yaxis = list(
           title = "1"))

fig


# By scene
friends_scene <- friends_data %>% group_by(scene, episode, season) %>% summarise(text = paste0(text, collapse = ""))  %>% arrange(season, episode, scene)



friends_scene$doc_id <- gsub(" ", paste0(friends_scene$season, friends_scene$episode, friends_scene$scene),replacement =  "")
model <- paragraph2vec(x = friends_scene, type = "PV-DBOW", dim = 300, iter = 50)
embedding <- as.matrix(model)
friends_2d <- umap(embedding, n_neighbors = 15, n_threads = 4)
friends_2d <- as.data.frame(friends_2d)
friends_2d <-tibble::rownames_to_column(friends_2d)
names(friends_2d)<- c("doc_id","x1", "x2")
friends_scene <-friends_scene %>% inner_join(friends_2d, friends_scene, by= c("doc_id"= "doc_id"))



fig <- plot_ly(friends_scene, x = ~x1, y = ~x2, type = 'scatter', mode = 'markers', color = friends_scene$season, text = friends_scene$season) %>%
  layout(plot_bgcolor = "#e5ecf6",
         xaxis = list(
           title = "0"),
         yaxis = list(
           title = "1"))

fig




