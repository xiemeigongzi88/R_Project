library(SparkR)
library(tm)
library(dplyr)

###  inital spark
sparkR.session()

'''
corpus <- read.delim("C:/Users/achen/Downloads/MovieSummaries/MovieSummaries/plot_summaries.txt"
                   , header = FALSE, sep = "\t", quote = "")
'''
corpus<-plot_summaries
names(corpus)[1] <- paste("doc_id")
names(corpus)[2] <- paste("text")

query <- "frident happy must"

qd <- data.frame("query", query)
names(qd) <- c("doc_id", "text")

#newdf <- rbind(corpus, qd)

#vtmp <- DataframeSource(newdf)
#vcorpus<- Corpus(vtmp)

vtmp <- DataframeSource(corpus)
vcorpus<- Corpus(vtmp)

## remove stopwords
df <-tm_map(vcorpus, stripWhitespace)

df <- tm_map(df, removeNumbers)

df <- tm_map(df, removePunctuation)

df <- tm_map(df, content_transformer(tolower))

df <- tm_map(df, removeWords, stopwords("english"))



## stem words
df<- tm_map(df, stemDocument)

N.docs <- dim(corpus)

## -----------------------------------------------
## create query 

vquery <- DataframeSource(qd)
vquery<- Corpus(vquery)

qdf <-tm_map(vquery, stripWhitespace)

qdf <- tm_map(qdf, removeNumbers)

qdf <- tm_map(qdf, removePunctuation)

qdf <- tm_map(qdf, content_transformer(tolower))

qdf <- tm_map(qdf, removeWords, stopwords("english"))

## stem query words
qdf<- tm_map(qdf, stemDocument)

inspect(qdf)
#--------------------------------------------------


 ## combine data and query
text <- c()
doc_id <- c()

for( i in 1:N.docs[1] ){
  text[i] <- df[[i]]$content
  doc_id[i] <- df[[i]]$meta$id
}
text[N.docs[1]+1] <- qdf[[1]]$content
doc_id[N.docs[1]+1] <- "query"

allDataframe <- data.frame(doc_id, text)

View(allDataframe[1:6,])

 ## 
vtmp <- DataframeSource(allDataframe)
vcorpus<- Corpus(vtmp)


## create term-document matrix
tdm <- TermDocumentMatrix(vcorpus)
tmp <- removeSparseTerms(tdm, 0.96)
inspect(tdm[0:4,])
inspect(tmp[0:4,])

term.doc.matrix <- as.matrix(tmp)

most_frecuent_matrix<- addmargins(term.doc.matrix, margin = 2) 
most_frecuent_matrix<- most_frecuent_matrix[order(most_frecuent_matrix[,42308], decreasing = TRUE),] 
most_frecuent_matrix_top10<- head(most_frecuent_matrix[,c(42300,42308)], n = 10)
print(most_frecuent_matrix_top10)

## construct the vector space model
weights <- function(tf.vec) {
  # Compute tfidf weights from term frequency vector
  n.docs <- length(tf.vec)
  doc.frequency <- length(tf.vec[tf.vec > 0])
  weights <- rep(0, length(tf.vec))
  weights[tf.vec > 0] <- (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/doc.frequency)
  return(weights)
}

tfidf.matrix <- t(apply(term.doc.matrix, 1,
                        FUN = function(row) {weights(row)}))

colnames(tfidf.matrix) <- colnames(term.doc.matrix)

## view tf-idf matrix
tfidf.matrix[0:4, 1:9]
term.doc.matrix[0:4,1:9]


## get related result
query.vector <- tfidf.matrix[, (N.docs+1)]
tfidf.matrix <- tfidf.matrix[, 1: N.docs]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = colnames(tfidf.matrix), score = t(doc.scores))

results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)
