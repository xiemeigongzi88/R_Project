# import dataset from UCI 
roman<-read.csv("RomanUrduDataSet.csv",header = FALSE,stringsAsFactors = FALSE)

check_1<-as.factor(roman$V2)
print(levels(check))

test<-roman$V2
test[test<-(test=="Neative")]<-"Negative"

check_2<-as.factor(test)
print(levels(check_2))

# store the texts to "data"
data<-roman$V1

# store labels to "label_trail"
label_trail<-as.factor(test)
label_fact<-factor(label_trail,labels=c(2,1,0))
label<-as.numeric(levels(label_fact))[label_fact]


library(tm)
library(SnowballC)

# transform into the format that tm can process
corpus <- VCorpus(VectorSource(data))
corpus <- tm_map(corpus, tolower)
#  remove all metadata that we don't need
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
# remove stopwords
corpus <- tm_map(corpus, removeWords, c(stopwords("ro")))
# remember to import "SnowballC" library before stemming
corpus <- tm_map(corpus, stemDocument)
# generate the default document-term matrix with bag-of-words and term frequency
dtm <- DocumentTermMatrix(corpus)

#Learning hidden representation of deep neural network
rownames(dtm) <- c(1:nrow(roman))

d <- data.frame(as.matrix(dtm))
d$label <- label

# install package 
devtools::install_github("rstudio/keras")
library(keras)
install.packages("tensorflow")
install_tensorflow()

## The following objects are masked from 'package:text2vec':
## 
##     fit, normalize
maxlen <- 200
encoding_dim <- 32
batch_size <- 10
epochs <- 50
# batch_size and epochs are training parameters, you may change them to get better results


tok <- text_tokenizer(2000, lower = TRUE, split = " ", char_level = FALSE)
fit_text_tokenizer(tok, data)
data_idx <- texts_to_sequences(tok, data)

data_idx <- data_idx %>%
  pad_sequences(maxlen = maxlen)

head(data_idx)
#dim(data_idx)

#Then we split the data into training (70%) and validation sets (30%).
inTraining <- 1:floor(nrow(data_idx) * 0.7)
x_train <- data_idx[inTraining, ]
x_test <- data_idx[-inTraining, ]

x_train <-array_reshape(x_train, c(14160,200))
x_test<-array_reshape(x_test,c(6069,200))

y_train<-label[inTraining]
y_test<-label[-inTraining]


# initialize the model
model <- keras_model_sequential()
model %>%
  layer_dense(name = 'e1', units = encoding_dim, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 64, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 128, activation = 'relu') %>%
  layer_dense(name = 'd1', units = 64, activation = 'relu') %>%
  layer_dense(name = 'd2', units = 128, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')


model %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

#summary(model)


history <- model %>% fit(
  x_train, y_train ,
  batch_size = batch_size,
  epochs = epochs
  #shuffle = TRUE, 
  #validation_data = list(x_test, y_test)
)

plot(history)

score<-model %>% evaluate(x_test,y_test)

predicts<-model %>% predict(x_test)

model %>% evaluate(x_test, y_test)


##########################################
# vary the parameters _1 
library(keras)
maxlen_1 <- 200
encoding_dim_1 <- 32
batch_size_1 <- 2
epochs_1 <- 50
model_1 <- keras_model_sequential()
model_1 %>%
  layer_dense(name = 'e1', units = encoding_dim_1, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 64, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_1 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_1 <- model_1 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_1,
  epochs = epochs_1
)

plot(history_1)

model_1 %>% evaluate(x_test, y_test)

##########################################
# vary the parameters _2
maxlen_2 <- 200
encoding_dim_2 <- 64
batch_size_2 <- 10
epochs_2 <- 50
model_2 <- keras_model_sequential()
model_2 %>%
  layer_dense(name = 'e1', units = encoding_dim_1, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 128, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_2 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_2 <- model_2 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_2,
  epochs = epochs_2
)

plot(history_2)

model_2 %>% evaluate(x_test, y_test)


#########################################################
# vary the parameters _3
maxlen_3 <- 200
encoding_dim_3 <- 128
batch_size_3 <- 10
epochs_3 <- 50
model_3 <- keras_model_sequential()
model_3 %>%
  layer_dense(name = 'e1', units = encoding_dim_3, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 256, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_3 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_3 <- model_3 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_3,
  epochs = epochs_3
)

plot(history_3)

model_3 %>% evaluate(x_test, y_test)

############################################
# vary the parameters _4
maxlen_4 <- 200
encoding_dim_4 <- 32
batch_size_4 <- 10
epochs_4 <- 50
model_4 <- keras_model_sequential()
model_4 %>%
  layer_dense(name = 'e1', units = encoding_dim_4, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 256, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 256, activation = 'relu') %>%
  layer_dense(name = 'e4', units = 256, activation = 'relu') %>%
  layer_dense(name = 'e5', units = 256, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_4 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_4 <- model_4 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_4,
  epochs = epochs_4
)

plot(history_4)

model_4 %>% evaluate(x_test, y_test)

##################################################
# vary the parameters _5
maxlen_5 <- 200
encoding_dim_5 <- 32
batch_size_5 <- 10
epochs_5 <- 50
model_5 <- keras_model_sequential()
model_5 %>%
  layer_dense(name = 'e1', units = encoding_dim_5, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 64, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 128, activation = 'relu') %>%
  layer_dense(name = 'e4', units = 128, activation = 'relu') %>%
  layer_dense(name = 'e5', units = 64, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_5 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_5 <- model_5 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_5,
  epochs = epochs_5
)

plot(history_5)

model_5 %>% evaluate(x_test, y_test)


################################################
# vary the parameters _6
maxlen_6 <- 200
encoding_dim_6 <- 32
batch_size_6 <- 10
epochs_6 <- 50
model_6 <- keras_model_sequential()
model_6 %>%
  layer_dense(name = 'e1', units = encoding_dim_4, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 64, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 128, activation = 'relu') %>%
  layer_dense(name = 'e4', units = 256, activation = 'relu') %>%
  layer_dense(name = 'e5', units = 128, activation = 'relu') %>%
  layer_dense(name = 'e6', units = 64, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_6 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_6 <- model_6 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_6,
  epochs = epochs_6
)

plot(history_6)

model_6 %>% evaluate(x_test, y_test)

##############################################
# vary the parameters _7
maxlen_7 <- 300
encoding_dim_7 <- 4
batch_size_7 <- 10
epochs_7 <- 50
model_7 <- keras_model_sequential()
model_7 %>%
  layer_dense(name = 'e1', units = encoding_dim_7, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 16, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 32, activation = 'relu') %>%
  layer_dense(name = 'e5', units = 16, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_7 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_7 <- model_7 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_7,
  epochs = epochs_7
)

plot(history_7)

model_7 %>% evaluate(x_test, y_test)

#########################################3
# vary the parameters _8
maxlen_8 <- 300
encoding_dim_8 <- 4
batch_size_8 <- 10
epochs_8 <- 50
model_8 <- keras_model_sequential()
model_8 %>%
  layer_dense(name = 'e1', units = encoding_dim_8, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 8, activation = 'relu') %>%
  layer_dense(name = 'e3', units = 16, activation = 'relu') %>%
  layer_dense(name = 'e4', units = 8, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_8 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_8 <- model_8 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_8,
  epochs = epochs_8
)

plot(history_8)

model_8 %>% evaluate(x_test, y_test)


##############################################
# vary the parameters _9
maxlen_9 <- 300
encoding_dim_9 <- 4
batch_size_9 <- 10
epochs_9 <- 50
model_9 <- keras_model_sequential()
model_9 %>%
  layer_dense(name = 'e1', units = encoding_dim_9, activation = 'relu',input_shape = 200 ) %>%
  layer_dense(name = 'e2', units = 8, activation = 'relu') %>%
  layer_dense(name = 'd3', units = 3, activation = 'sigmoid')

model_9 %>% compile(
  loss="sparse_categorical_crossentropy", 
  optimizer='adam', 
  metrics='accuracy'
)

history_9 <- model_9 %>% fit(
  x_train, y_train ,
  batch_size = batch_size_9,
  epochs = epochs_9
)

plot(history_9)

model_9 %>% evaluate(x_test, y_test)




################################################

# supplementry 
# show the most frequent words

dtm_tfidf <- DocumentTermMatrix(corpus,
                                control=list(weighting=function(x) weightTfIdf(x, normalize=TRUE),
                                             stopwords=TRUE))
dtm_tfidf

findFreqTerms(dtm, lowfreq=300)

# generate the document-term matrix with bag-of-words and tf-idf weighting
dtm_tfidf <- DocumentTermMatrix(corpus,
                                control=list(weighting=function(x) weightTfIdf(x, normalize=TRUE),
                                             stopwords=TRUE))

# show the most "important" words
findFreqTerms(dtm_tfidf, lowfreq=2.5)

# generate the document-term matrix with bigram and term frequency
BigramTokenizer <- function(x) 
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm_bigram <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
dtm_bigram

findFreqTerms(dtm_bigram, lowfreq=100)

dtm <- removeSparseTerms(dtm, 0.995)

findAssocs(dtm, "ventricular", corlimit=0.5)

library(wordcloud)
# convert a sparse matrix into a data frame
df <- suppressWarnings(data.frame(as.matrix(dtm)))
# create a word cloud
wordcloud(colnames(df), colSums(df), scale=c(5, 1), max.words=50, min.freq=10, 
          color=brewer.pal(6, "Dark2"))

