

library(friends)
library(dplyr)
library(tidytext)
library(wordcloud)
library(reshape2)


dplyr::glimpse(friends)
friends_data <- friends::friends


friends_data %>%
  filter(speaker == "Monica Geller" | speaker == "Joey Tribbiani" | speaker == "Chandler Bing" | speaker == "Phoebe Buffay"
           | speaker =="Ross Geller") %>%
  group_by(speaker) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  arrange(desc(n), .by_group = T)  %>%
  acast(word ~ speaker, value.var = "n", fill = 0) %>%
  comparison.cloud( title.size = 1, random.order = FALSE, max.words = 500)






#devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

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
  unnest_tokens(bigram,text,token = "ngrams", n = 3 ) %>%
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





