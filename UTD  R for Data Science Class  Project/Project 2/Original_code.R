knitr::opts_chunk$set(echo = TRUE)

library(keras)
install.packages("stringr", dependencies = TRUE)
library(stringr)
library(NLP)
library(tm)
library(SnowballC)
library(plyr)
library(cloudml)

# Number of words to consider as features
max_features <- 10000

# Cut texts after this number of words

maxlen <- 20
# Load the data as lists of integers.
#data <-read.csv2(file = "data.csv", header=TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

url <- "http://www.utdallas.edu/~yxz179830/"
url <- paste(url, "data.csv", sep = "")
data <- read.csv(url, header = TRUE)

colnames(data)

dim(data)

# distribution of the ratings
#hist(data$reviews.rating)

# rating=5 -> pleased -> 1
# rating=1,2,3,4 -> average -> 0
typeof(data$reviews.rating)

data$target = ifelse(data$reviews.rating==5, '1',  '0')


new_data <- data[,c("target", "reviews.text")]


#######clear review #########

#remove stop words

review <- new_data$reviews.text
review <- Corpus(VectorSource(review))

corpus <- tm_map(review, tolower)
corpus<- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords('english'))

corpus <- tm_map(corpus, stemDocument)

#View(corpus)

#create data as lists of integers.

#create data frame with clear data
haha <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

#View(haha)

# split data to training data and test data

size <- floor(0.8 * nrow(haha))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(haha)), size = size)

# get train data and train data
train_x <- haha[train_ind, ]
test_x <- haha[-train_ind, ]
train <- new_data[train_ind, ]
test <-  new_data[-train_ind, ]

y_train <- train$target
y_test <- test$target

# convert training data to integers list
tok <- text_tokenizer(2000, lower = TRUE, split = " ", char_level = FALSE)
fit_text_tokenizer(tok, train_x)
train_data_idx <- texts_to_sequences(tok, train_x)

   ##View(train_data_idx)


# convert test data to integers list
fit_text_tokenizer(tok, test_x)
test_data_idx <- texts_to_sequences(tok, test_x)

  ##View(test_data_idx)

# This turns our lists of integers
# into a 2D integer tensor of shape `(samples, maxlen)`
x_train <- pad_sequences(train_data_idx, maxlen = maxlen)
x_test <- pad_sequences(test_data_idx, maxlen = maxlen)


##### text embedding model   #############

tb_model <- keras_model_sequential() %>%
  # We specify the maximum input length to our Embedding layer
  # so we can later flatten the embedded inputs
  layer_embedding(input_dim = 10000, output_dim = 8,
                  input_length = maxlen) %>%
  # We flatten the 3D tensor of embeddings
  # into a 2D tensor of shape `(samples, maxlen * 8)`
  layer_flatten() %>%
  # We add the classifier on top
  layer_dense(units = 1, activation = "sigmoid")
tb_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

tb_history <- tb_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2
)
plot(tb_history)

tb_score<- tb_model %>% evaluate(x_test,y_test)

tb_predicts<- tb_model %>% predict(x_test)

tb_predict <- list()

for (i in 1 : dim(tb_predicts)[1]) {
  tb_predict[i] <-round(tb_predicts[i])
}

my_tb <- as.data.frame(tb_predict)

# write acutal label of test data
#write.csv(y_test, "actual_label.csv", row.names=FALSE)

# get predict from data model and write to file
my_tb <- setNames(do.call(rbind.data.frame, my_tb), c("predict"))
#write.csv(my_tb, "text_modeling.csv", row.names=FALSE)


########## RNN layer ############
rnn_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")
rnn_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
rnn_history <- rnn_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)
plot(rnn_history)

rnn_score<- rnn_model %>% evaluate(x_test,y_test)

rnn_predicts<- rnn_model %>% predict(x_test)

rnn_predict <- list()

for (i in 1 : dim(rnn_predicts)[1]) {
  rnn_predict[i] <-round(rnn_predicts[i])
}

my_rnn <- as.data.frame(rnn_predict)

# get predict from data model and write file
my_rnn <- setNames(do.call(rbind.data.frame, my_rnn), c("predict"))
write.csv(my_rnn, "rnn_e10_sigmoid.csv", row.names=FALSE)

#-----------------------------------------------------------------

rnn_model1 <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 16) %>%
  layer_simple_rnn(units = 16) %>%
  layer_dense(units = 1, activation = "relu")
rnn_model1 %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
rnn_history1 <- rnn_model1 %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 128,
  validation_split = 0.2
)
plot(rnn_history1)

rnn_score1 <- rnn_model1 %>% evaluate(x_test,y_test)

rnn_predicts1<- rnn_model1 %>% predict(x_test)

rnn_predict1 <- list()

for (i in 1 : dim(rnn_predicts1)[1]) {
  rnn_predict1[i] <-round(rnn_predicts1[i])
}

my_rnn1 <- as.data.frame(rnn_predict1)

# get predict from data model and write to file
my_rnn1 <- setNames(do.call(rbind.data.frame, my_rnn1), c("predict"))
#write.csv(my_rnn, "rnn_e20_relu.csv", row.names=FALSE)


#-----------------------------------------------------------------
rnn_model2 <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>%
  layer_dense(units = 1, activation = "tanh")
rnn_model2 %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
rnn_history2 <- rnn_model2 %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)
plot(rnn_history2)

rnn_score2 <- rnn_model2 %>% evaluate(x_test,y_test)

rnn_predicts2 <- rnn_model2 %>% predict(x_test)

rnn_predict2 <- list()

for (i in 1 : dim(rnn_predicts2)[1]) {
  rnn_predict2[i] <-round(rnn_predicts2[i])
}

my_rnn2 <- as.data.frame(rnn_predict2)

# get predict from data model
my_rnn2 <- setNames(do.call(rbind.data.frame, my_rnn), c("predict"))
#write.csv(my_rnn2, "rnn_e10_tanh.csv", row.names=FALSE)

###########  LSTM layer ############

lstm_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")
lstm_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
lstm_history <- lstm_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)
plot(lstm_history)

lstm_score<- lstm_model %>% evaluate(x_test,y_test)

lstm_predicts<- lstm_model %>% predict(x_test)

lstm_predict <- list()

for (i in 1 : dim(lstm_predicts)[1]) {
    lstm_predict[i] <-round(lstm_predicts[i])
}

my_lstm <- as.data.frame(lstm_predict)

my_lstm <- setNames(do.call(rbind.data.frame, my_lstm), c("predict"))
#write.csv(my_rnn, "lstm_e10_sigmoid.csv", row.names=FALSE)

cloudml_train("code.R")
