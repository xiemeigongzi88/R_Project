# trial_5
library(twitteR)
library(tidytext)
library(dplyr)
library(tidyr)
library(tm)
library(SnowballC)

library(SparkR)

library(tidyverse)
###  inital spark
#sparkR.session()

data<-plot_summaries
#View(data)
n<-100

text<-data$V2[1:100]
rowIndex<-c(1:n)
docRelated<-data$V1[1:100]

data<-data.frame("rowIndex"=rowIndex,"text"=text,"docRelated"=docRelated)

docList<-as.list(text)
N.docs<-length(docList)

##########################################################################
QrySearch <- function(queryTerm) {
  
  # Record starting time to measure your search engine performance
  start.time <- Sys.time()
  
  # store docs in Corpus class which is a fundamental data structure in text mining
  my.docs <- VectorSource(c(docList, queryTerm))
  
  
  # Transform/standaridze docs to get ready for analysis
  my.corpus <- VCorpus(my.docs) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  
  # Store docs into a term document matrix where rows=terms and cols=docs
  # Normalize term counts by applying TDiDF weightings
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
                                            control=list(
                                              weighting=function(x) weightSMART(x,spec="ltc"),
                                              wordLengths=c(0,Inf)))
  
  
  
  # Transform term document matrix into a dataframe
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    dplyr::group_by(document) %>% 
    dplyr::mutate(vtrLen=sqrt(sum(count^2))) %>% 
    dplyr::mutate(count=count/vtrLen) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(term:count)
  docMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document>=N.docs+1)
  
  
  
  
  
  # Calcualte top ten results by cosine similarity
  searchRes <- docMatrix %>% 
    dplyr::inner_join(qryMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    dplyr::mutate(termScore=round(count.doc*count.query,4)) %>% 
    dplyr::group_by(document.query,document.doc) %>% 
    dplyr::summarise(Score=sum(termScore)) %>% 
    dplyr::filter(row_number(desc(Score))<=10) %>% 
    dplyr::arrange(desc(Score)) %>% 
    dplyr::left_join(data,by=c("document.doc"="rowIndex")) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(Result=text) %>% 
    dplyr::select(Score,docRelated) %>% 
    data.frame()
  
  
  # Record when it stops and take the difference
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,4)
  print(paste("Used",time.taken,"seconds"))
  
  return(searchRes)
  
}