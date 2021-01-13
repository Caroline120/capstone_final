# Capstone Week 2 Project: Milestone Report
#caroline Doe
#december,2020
#Executive Summary
# The objectives for this project is to: 1. Demonstrate that I've downloaded the data and have successfully loaded it in. 2. Create a basic report of summary statistics about the data sets. 3. Report any interesting findings that I found so far.
#In my project, I will be performing the following steps: 1. Load the data 2. Take random samples of the data to make this initial look go faster. 3. Some initial cleaning of the data 4. Pull some basic statistics 5. Create some simple bar plots and word clouds for 1-4 word sequences 6. Summary my findings and talk about what I will likely do going forward thru the rest of the class.
Corpus
#Loading the sample files from disk and clean the data by removing 

Wordcloud
#To examine the data, we will produce a word clouds showing frequently used terms in the datasets. 
#The word clouds show generally the top words with size varying by frequency.
# load packages
setwd("C:\\Users\\caro\\Desktop\\directory")
library(dplyr)
library(data.table)
library(qdap)
library(ngram)
library(tm)
library(RWeka)
library(stringr)
library(stringi)
library(NLP)
library(wordcloud)
library(ggplot2)


blog <- readLines("en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("en_US.twitter.txt" , warn = FALSE, encoding = "UTF-8")                            

# Sample the files to provide a faster look at the data files
#For this, I chose to take a random sample of 1% of each file. I had initially tried 10% but ended up with a corpus that was about 1.3 gigabytes of data and was still taking an extensive period of time to run through the various steps.
#This is to allow us to provide a faster look at the data to see what we see and decide on the next steps before analyzing the entire data files.

set.seed(671) 
sample_size = 1500

blog_sample <- blog[sample(1:length(blog),sample_size)]
news_sample <- news[sample(1:length(news),sample_size)]
twitter_sample <- twitter[sample(1:length(twitter),sample_size)]
                     
                    
sample_set<-rbind(blog_sample,news_sample,twitter_sample)
rm(blog,news,twitter)

#clean the sample_set, create corpus
corpus<-VCorpus(VectorSource(sample_set))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
changetospace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, changetospace, "/|@|\\|")

#process NGram Tokenizer
ugt <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bgt <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tgt <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
qgt <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

#process Term Document Matrix
uniTDM <- TermDocumentMatrix(corpus, control = list(tokenize = ugt))
biTDM <- TermDocumentMatrix(corpus, control = list(tokenize = bgt))
triTDM <- TermDocumentMatrix(corpus, control = list(tokenize = tgt))
quadTDM <- TermDocumentMatrix(corpus, control = list(tokenize = qgt))

#find frequency terms

#unigram frequency
ft1 <- findFreqTerms(uniTDM, lowfreq = 4)
tFreq1 <- rowSums(as.matrix(uniTDM[ft1,]))
tFreq1 <- data.frame(unigram=names(tFreq1), frequency=tFreq1)
tFreq1 <- tFreq1[order(-tFreq1$frequency),]
u_list <- setDT(tFreq1)
saveRDS(u_list,file="unigram.RData")

#bigram frequency
ft2 <- findFreqTerms(biTDM, lowfreq = 3)
tFreq2 <- rowSums(as.matrix(biTDM[ft2,]))
tFreq2 <- data.frame(bigram=names(tFreq2), frequency=tFreq2)
tFreq2 <- tFreq2[order(-tFreq2$frequency),]
b_list <- setDT(tFreq2)
saveRDS(b_list,file="bigram.RData")

#trigram frequency
ft3 <- findFreqTerms(triTDM, lowfreq = 3)
tFreq3 <- rowSums(as.matrix(triTDM[ft3,]))
tFreq3 <- data.frame(trigram=names(tFreq3), frequency=tFreq3)
tFreq3 <- tFreq3[order(-tFreq3$frequency),]
t_list <- setDT(tFreq3)
saveRDS(t_list, file="trigram.RData")

#quadgram frequency
ft4 <- findFreqTerms(quadTDM, lowfreq = 2)
tFreq4 <- rowSums(as.matrix(quadTDM[ft4,]))
tFreq4 <- data.frame(quadgram=names(tFreq4), frequency=tFreq4)
q_list <- setDT(tFreq4)
saveRDS(q_list,file="quadgram.RData")