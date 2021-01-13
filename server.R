library(shiny)

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))
suppressWarnings(library(tidyverse))

setwd("C:\\Users\\caro\\Desktop\\directory")
# Load Quadgram,Trigram & Bigram Data frame files

quadgram <- readRDS("quadgram.RData");
trigram <- readRDS("trigram.RData");
bigram <- readRDS("bigram.RData");
mesg <<- ""

# Cleaning of user input before predicting the next word
#Create User Input and Data Cleaning Function; Calls the matching functions
ngrams <- function(input_text){
  # Create a dataframe
  input_text <- data_frame(text = input_text)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input_text <- input_text %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input_text, boundary("word"))
  input_words <- unlist(str_split(input_text, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  
  nextWord <- ifelse(input_count == 0,"Please enter your word or phrase in the given left text box.",
                     ifelse(input_count == 1, bigram(input_words),
                            ifelse (input_count == 2, trigram(input_words),
                                    ifelse (input_count == 3, quadgram(input_words)))))
  if(nextWord == "?"){
    nextWord = "The application not found the next expected word due to limited size of the training data" 
  }
  return(nextWord)
}


## Create Ngram Matching Functions

bigram <- function(input_words){
  mesg<-"The word is predicted by 2-gram"
  num <- length(input_words)
  filter(bigram,
         bigram[,1]==input_words[num]) %>%
    head(2) %>%                            
    #filter(row_number() == 1L) %>%
    select(starts_with("bigram"))->x
  as.matrix(x)->y
  as.character(x)->out
  ifelse(out =="integer(0)", "?", ifelse(is.na(y[2]),paste(y[1],mesg,sep=" , "),paste(y[1],y[2],mesg,sep=" , ")))
  
}

trigram <- function(input_words){
  mesg<-"The word is predicted by 3-gram"
  num <- length(input_words)
  filter(trigram,
         trigram[,1]==input_words[num-1],
         trigram[,2]==input_words[num])  %>%
    head(2) %>%
    filter(row_number() == 1L) %>%
    select(starts_with("tri"))->x
  as.matrix(x)->y
  as.character(x)-> out
  ifelse(out =="integer(0)", bigram(input_words[num]), ifelse(is.na(y[2]),paste(y[1],mesg,sep=" , "),paste(y[1],y[2],mesg,sep=" , ")))
  
}

quadgram <- function(input_words){
  mesg<-"The word is predicted by 4-gram"
  num <- length(input_words)
  filter(quadgram,
         quadgram[,1]==input_words[num-2],
         quadgram[,2]==input_words[num-1],
         quadgram[,3]==input_words[num])  %>%
    head(2) %>%
    filter(row_number() == 1L) %>%
    select(starts_with("qua"))-> x
  as.matrix(x)->y
  as.character(x)-> out
  a<-paste(input_words[num-1],input_words[num])
  a <- unlist(str_split(a, boundary("word")))
  ifelse(out=="integer(0)", trigram(a),ifelse(is.na(y[2]),paste(y[1],mesg,sep=" , "),paste(y[1],y[2],mesg,sep=" , ")))
}

shinyServer(function(input, output){
  output$prediction <- renderPrint({
    next_word <- ngrams(input$inputString)
    next_word
  })
  
  output$text1 <- renderText({
    input$inputString
  })
  
  
})