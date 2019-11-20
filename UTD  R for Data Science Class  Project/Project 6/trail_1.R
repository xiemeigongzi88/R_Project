'''
1. First of all you would need to remove stop words, which can be
done by searching for an appropriate package. Some
suggestions are:
'''

library(tm)

data<-plot_summaries

View(data)

col_1<-data$V1
col_2<-data$V2

col_2_vector<-VectorSource(col_2)

col_2_corpus<-VCorpus(col_2_vector)

#View(col_2_corpus[1])


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

#query <- "Marvel"

#qd <- data.frame("query", query)
#names(qd) <- c("doc_id", "text")

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


##########################################
# suplimentry  
'''
4. After the above pre-processing steps, you are ready to compute
tf-idf values for each document-term pair. You can save this for
faster processing next time
'''

library(dplyr)
library(tidytext)


#############################################
'''
4. Your program should read query phrases (which could be
multiple terms such as “action movies with comedy scenes”)
from the command line and should return the top 10
documents matching the user’s queries. The program should
terminate when the user presses the “q” key.

'''











