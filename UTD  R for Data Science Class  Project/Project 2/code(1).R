library(keras)
install.packages("stringr", dependencies = TRUE)
library(stringr)
library(NLP)
library(tm)
library(SnowballC)
library(plyr)

# Number of words to consider as features
max_features <- 10000

# Cut texts after this number of words

maxlen <- 20
# Load the data as lists of integers.
data <-read.csv2(file="data.csv", header=TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

colnames(data)

dim(data)

# distribution of the ratings
hist(data$reviews.rating)

# rating=5 -> pleased -> 2
# rating=3,4 -> average -> 1
# rating=1,2 -> not goog -> 0
typeof(data$reviews.rating)

data$target = ifelse(data$reviews.rating==5, 2, 
                     ifelse(data$reviews.rating %in% c(3,4), 1, 0))


# remove punctuation


#split data into training data and test data

size <- floor(0.8 * nrow(data))

  ## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = size)

new_data <- data[,c("target", "reviews.text")]

# get train data and train data
train <- new_data[train_ind, ]
test <- new_data[-train_ind, ]

# clear reviews.text
#remove stop words

train_review <- train$reviews.text
review <- Corpus(VectorSource(train_review))

x_train <- tm_map(review, tolower)
x_train <- tm_map(x_train, PlainTextDocument)
x_train<- tm_map(review, removePunctuation)
x_train<- tm_map(x_train, removeNumbers)
x_train<- tm_map(x_train, stripWhitespace)

x_train <- tm_map(x_train, removeWords, stopwords('english'))

x_train <- tm_map(x_train, stemDocument)

View(x_train)

#create data as lists of integers.
# the dimension of x_train

haha <- data.frame(text = sapply(x_train, as.character), stringsAsFactors = FALSE)

View(haha$text)

tok <- text_tokenizer(2000, lower = TRUE, split = " ", char_level = FALSE)
fit_text_tokenizer(tok, haha$text)
data_idx <- texts_to_sequences(tok, haha$text)

View(data_idx)

unletter <- function(word) {
    gsub('-64',' ',paste(sprintf("%02d",utf8ToInt(tolower(word)) -  
                            96),collapse='')) }

sum(is.na(haha))

a <- strsplit(haha[1,], " ")
unletter(a[[1]][1])
as.double(unletter(a[[1]][1]))

#ha[] <- lapply(haha, unletter)

c <- list()

for (j in 1:dim(haha)[1]) {
  a <- strsplit(haha[j,], " ")
  for (i in 1:length(a[[1]])) {
    li[i] <- as.double(unletter(a[[1]][i]))
  }
  c <- li
}


y_train <- train$target

x_test <- test$reviews.text
y_test <- test$target

typeof(x_train)

# This turns our lists of integers
# into a 2D integer tensor of shape `(samples, maxlen)`
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)



library(tm)
x <- c("Hello. Sir!","Tacos? On Tuesday?!?")
mycorpus <- Corpus(VectorSource(x))
mycorpus <- tm_map(mycorpus, removePunctuation)
View(mycorpus)
dataframe <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), 
                        stringsAsFactors=F)
View(dataframe)


