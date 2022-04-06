library(caret)
library(dplyr)
library(doc2vec)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.classifiers)
library(udpipe)
library(glmnet)
library(plotly)
library(ROCR)
movie <- readr::read_csv('C:\\Users\\Drecker\\Documents\\Lehre\\Daten\\movie_review.csv')
movie <- movie %>% select( text,tag) 


movie_cor <- quanteda::corpus(movie)  %>%
  corpus_trim(
    what = "documents",
    min_ntoken = 3
  )

lemma_en <- udpipe_download_model(language = "english")
lemma_en <- udpipe_load_model(file = lemma_en$file_model)
lemma_en  <- udpipe(movie_cor, lemma_en, parallel.cores = 8)
lemma_en <- lemma_en %>% filter(upos != 'PUNCT' & is.na(lemma) == F)

movie_cor <- quanteda::corpus(movie)  %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove( c(stopwords("english"))) %>%
  tokens_ngrams( n = 1)



  
movie_cor_le <-   tokens_replace(tokens(movie_cor), pattern = lemma_en$token, replacement = lemma_en$lemma)

movie_cor[1]
movie_cor_le[1]




training_id <- sample(1:nrow(movie), 0.66 * nrow(movie),replace = FALSE)
docvars(movie_cor_le, "id_numeric") <- 1:ndoc(movie_cor)

movie_cor_training <-movie_cor_le %>%
  dfm() %>%
  dfm_subset(id_numeric %in% training_id)



movie_cor_test <- movie_cor_le %>%
  tokens() %>%
  dfm() %>%
  dfm_subset(!id_numeric %in% training_id)
  
  
print(prop.table(table(docvars(
  movie_cor_training, "tag"
))) * 100)


print(prop.table(table(docvars(
  movie_cor_test, "tag"
))) * 100)







lasso <- cv.glmnet(x = movie_cor_training,
                   y = as.integer(movie_cor_training$tag == "pos"),
                   alpha = 1,
                   nfold = 5,
                   family = "binomial", type.measure = 'class')

plot(lasso)

lasso$cvm[lasso$lambda==lasso$lambda.min]




true_class <-as.integer(movie_cor_test$tag == 'pos')
predicted_class <- predict(lasso,  movie_cor_test, type = "class", s = lasso$lambda.min)
tab_class <- table(true_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything")

predicted_class <- predict(lasso, movie_cor_test,type="response")
pred_movie <- prediction(as.numeric(predicted_class), movie_cor_test$tag)  
perf_movie <- performance(pred_movie,"tpr","fpr") 


data_roc <- data.frame(x = perf_movie@x.values, y = perf_movie@y.values)
names(data_roc) <- c('x', 'y')

plot_ly(data = data_roc ,x =  ~x, y = ~y, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(title = 'ROC_Curve',xaxis = list(title = "False Positive Rate"), yaxis = list(title = "True Positive Rate")) %>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'),inherit = FALSE, showlegend = FALSE)




#######N Bayes

model_NB <-textmodel_nb(movie_cor_training, movie_cor_training$tag, prior = "docfreq")


crossval(model_NB, k = 10, by_class = F)



true_class <-movie_cor_test$tag
predicted_class <- predict(model_NB, newdata = movie_cor_test)
tab_class <- table(true_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything")



library(plotly)
library(ROCR)
predicted_class <- predict(model_NB, newdata = movie_cor_test,type="probability")
pred_movie <- prediction(as.numeric(predicted_class[,-1]), movie_cor_test$tag)  
perf_movie <- performance(pred_movie,"tpr","fpr") 


data_roc <- data.frame(x = perf_movie@x.values, y = perf_movie@y.values)
names(data_roc) <- c('x', 'y')

plot_ly(data = data_roc ,x =  ~x, y = ~y, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(title = 'ROC_Curve',xaxis = list(title = "False Positive Rate"), yaxis = list(title = "True Positive Rate")) %>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'),inherit = FALSE, showlegend = FALSE)






# Neuronales Netz
# Benötigt Anaconda
install.packages("tensorflow")
library(tensorflow)
library(dplyr)
library(ggplot2)
library(purrr)
library(keras)

friends_data <- friends::friends  %>%
  filter(speaker == "Monica Geller" | speaker == "Joey Tribbiani" | speaker == "Chandler Bing" | speaker == "Phoebe Buffay"
         | speaker =="Ross Geller")



class_df <- data.frame(speaker = c("Monica Geller", "Joey Tribbiani", "Ross Geller","Chandler Bing", "Phoebe Buffay", "Rachel Green"),
                       Class = c(1, 0, 2,0,0,0),
                       stringsAsFactors = FALSE)

friends_data <- merge(friends_data, class_df, by = "speaker", all.x = TRUE)



training_id <- sample.int(nrow(friends_data), size = nrow(friends_data)*0.8)
training <- friends_data[training_id,]
testing <- friends_data[-training_id,]


friends_data$text %>% 
  strsplit(" ") %>% 
  sapply(length) %>% 
  summary()


num_words <- 10000
max_length <- 30
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words, 
  output_sequence_length = max_length, 
)

text_vectorization %>% 
  adapt(friends_data$text)

text_vectorization(matrix(friends_data$text[1], ncol = 1))



input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 3, activation = "sigmoid")

model <- keras_model(input, output)



model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = list('accuracy')
)


history <- model %>% fit(
  training$text,
  training$Class,
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

results <- model %>% evaluate(testing$text, testing$Class, verbose = 0)
results



# Beispiel IMDB Dataset

df <- readr::read_csv('C:\\Users\\Drecker\\Documents\\Lehre\\Daten\\movie_review.csv')
head(df)
training_id <- sample.int(nrow(df), size = nrow(df)*0.8)
training <- df[training_id,]
testing <- df[-training_id,]
num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words, 
  output_sequence_length = max_length, 
)
text_vectorization %>% 
  adapt(df$text)
text_vectorization(matrix(df$text[1], ncol = 1))


input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)



model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)


history <- model %>% fit(
  training$text,
  as.numeric(training$tag == "pos"),
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

results <- model %>% evaluate(testing$text, as.numeric(testing$tag == "pos"), verbose = 0)
results

