# http://stackoverflow.com/questions/29376031/capture-the-label-of-an-actionbutton-once-it-is-clicked/29384955#29384955
#Load Library
library(tm)
library(openNLP) 
library(tm) 
library(RWeka) 
library(ggplot2) 
library(wordcloud)
library(reshape)
library(stringr)

shinyServer(function(input, output, session) {
  
  # Load Data
  bitData <- readRDS(file="dataFrameClean/bitGramTokenDF_clean_final.RDS")
  triData <- readRDS(file="dataFrameClean/triGramTokenDF_clean_final.RDS")
  quadData <- readRDS(file="dataFrameClean/quadGramTokenDF_clean_final.RDS")
  # Corpus and Suggestion Algorithms
  corpus2DataFrame <- function(dVec){
    options(mc.cores=6)
    corpus <- VCorpus(VectorSource(dVec)) # Building the main corpus
    corpus <- tm_map(corpus, removeNumbers) # removing numbers
    corpus <- tm_map(corpus, stripWhitespace) # removing whitespaces
    corpus <- tm_map(corpus, content_transformer(tolower)) #lowercasing all contents
    
    remove.slang <- content_transformer(function(x, pattern, replaceWith) gsub(pattern, replaceWith, x))
    corpus <- tm_map(corpus, remove.slang, "'ve", " have")
    corpus <- tm_map(corpus, remove.slang, "'s", " is")
    corpus <- tm_map(corpus, remove.slang, "'m", " am")
    corpus <- tm_map(corpus, remove.slang, "'ll", " will")
    corpus <- tm_map(corpus, remove.slang, "'re", " are")
    corpus <- tm_map(corpus, remove.slang, "can't", "can not")
    corpus <- tm_map(corpus, remove.slang, "n't", " not")
    corpus <- tm_map(corpus, remove.slang, "'d", " would")
    corpus <- tm_map(corpus, remove.slang, "'n ", "ing ")
    corpus <- tm_map(corpus, remove.slang, "&", " and")
    corpus <- tm_map(corpus, remove.slang, "[^[:alnum:][:blank:]]", "")
    
    corpus <- tm_map(corpus, removePunctuation) # removing special characters
    # badwords.txt from https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
    profanity <- readLines("badwords.txt", warn=FALSE)
    corpus <- tm_map(corpus, removeWords, profanity) 
    
    # convert from corpus to dataframe & save to RDS
    corpusDF <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
    corpusDF
  }
  topNextWord <- function(data, words, size){
    recommendList <- head(data[data$key==words,]$words, size)
    gsub(paste(words," ", sep=""), "", recommendList)
  }  
  suggestNextWord <- function(words, data, size)
  {
    splitWords <- unlist(strsplit(words, " "))
    splitWords <- splitWords[splitWords!=""]
    # find out what type of data
    dataSize <- length(unlist(strsplit(data$words[1], " ")))
    if(dataSize <= length(splitWords)){
      searchWords <- tail(splitWords, dataSize-1)
      searchText <- ""
      for(i in 1: length(searchWords)){
        if(i > 1){
          searchText <- paste(searchText, searchWords[i], sep=" ")
        }else{
          searchText <- searchWords[i]
        }
      }
      lowerCaseWords <- tolower(searchText)
      topNextWord(data, lowerCaseWords, size)
    }
  }
  
  # Load Storage
  myData <- reactiveValues(data = NULL, suggestionDataBit = NULL, suggestionDataTri = NULL, suggestionDataQuad = NULL)
  resetData <- function(){
    myData$data = NULL
  }
  getData <- function(){
    myData$data
  }
  setData <- function(newData){
    myData$data <- newData
  }
  getSuggestionData <- function(x){
    if(x=="bit")
      myData$suggestionDataBit
    else if(x=="tri")
      myData$suggestionDataTri
    else if(x=="quad")
      myData$suggestionDataQuad
  }
  setSuggestionData <- function(newSuggestionData, x){
    if(x=="bit")
      myData$suggestionDataBit <- newSuggestionData
    else if(x=="tri")
      myData$suggestionDataTri <- newSuggestionData
    else if(x=="quad")
      myData$suggestionDataQuad <- newSuggestionData
  }
  
  # Get inputText and trim leading and trailing whitespaces
  find.trailingWhiteSpace <- function (x) grep("\\s+$", x)
  trim.leadingTrailingWhiteSpace <- function (x) gsub("^\\s+|\\s+$", "", x)
  getInputText <- function(){
    input$text
  }
  getInputTextNoWhiteSpace <- function(){
    trim.leadingTrailingWhiteSpace(input$text)
  }
  hasTrailingWhiteSpace <- function(x){
    length(find.trailingWhiteSpace(x))==1
  }
  
  # You can access the value of the widget with input$text, e.g.
  output$valueText <- renderText({
    newInputText <- c(getInputTextNoWhiteSpace(), getData()) 
    resetData()
    newInputText
  })
  
  # Setup All reactive ObserveEvents
  ### Bit-Gram
  observeEvent(input$action2_1, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[1])
  })
  observeEvent(input$action2_2, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[2])
  })
  observeEvent(input$action2_3, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[3])
  })
  observeEvent(input$action2_4, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[4])
  })
  observeEvent(input$action2_5, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[5])
  })
  observeEvent(input$action2_6, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[6])
  })
  observeEvent(input$action2_7, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[7])
  })
  observeEvent(input$action2_8, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[8])
  })
  observeEvent(input$action2_9, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[9])
  })
  observeEvent(input$action2_10, {
    suggestionWords <- getSuggestionData("bit")
    setData(suggestionWords[10])
  })
  ### Tri-Gram
  observeEvent(input$action3_1, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[1])
  })
  observeEvent(input$action3_2, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[2])
  })
  observeEvent(input$action3_3, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[3])
  })
  observeEvent(input$action3_4, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[4])
  })
  observeEvent(input$action3_5, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[5])
  })
  observeEvent(input$action3_6, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[6])
  })
  observeEvent(input$action3_7, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[7])
  })
  observeEvent(input$action3_8, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[8])
  })
  observeEvent(input$action3_9, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[9])
  })
  observeEvent(input$action3_10, {
    suggestionWords <- getSuggestionData("tri")
    setData(suggestionWords[10])
  })
  ### Quad-Gram
  observeEvent(input$action4_1, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[1])
  })
  observeEvent(input$action4_2, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[2])
  })
  observeEvent(input$action4_3, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[3])
  })
  observeEvent(input$action4_4, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[4])
  })
  observeEvent(input$action4_5, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[5])
  })
  observeEvent(input$action4_6, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[6])
  })
  observeEvent(input$action4_7, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[7])
  })
  observeEvent(input$action4_8, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[8])
  })
  observeEvent(input$action4_9, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[9])
  })
  observeEvent(input$action4_10, {
    suggestionWords <- getSuggestionData("quad")
    setData(suggestionWords[10])
  })
  
  output$ui2 <- renderUI({
    inputText <- getInputTextNoWhiteSpace()
    actualInputText <- getInputText()
    if (inputText=="")
      return()
    else if(hasTrailingWhiteSpace(actualInputText)){
      # Trigger Suggestion Algorithm
      suggestedWords <- suggestNextWord(corpus2DataFrame(inputText)$text, bitData, 10)
      # Set to local storage
      setSuggestionData(suggestedWords, "bit")
      numSuggestedWords <- length(suggestedWords)
      if(numSuggestedWords > 0){
        myTagList <- tagList()
        for(i in 1:numSuggestedWords){
          actionBtnId <- paste("action2_", i, sep="")
          mySuggestedWord <- actionButton(actionBtnId, label = suggestedWords[i])
          myTagList <- tagAppendChild(myTagList, mySuggestedWord)
        }
        myTagList
      } 
    }
  })
  
  output$ui3 <- renderUI({
    inputText <- getInputTextNoWhiteSpace()
    actualInputText <- getInputText()
    if (inputText=="")
      return()
    else if(hasTrailingWhiteSpace(actualInputText)){
      # Trigger Suggestion Algorithm
      suggestedWords <- suggestNextWord(corpus2DataFrame(inputText)$text, triData, 10)
      # Set to local storage
      setSuggestionData(suggestedWords, "tri")
      numSuggestedWords <- length(suggestedWords)
      if(numSuggestedWords > 0){
        myTagList <- tagList()
        for(i in 1:numSuggestedWords){
          actionBtnId <- paste("action3_", i, sep="")
          mySuggestedWord <- actionButton(actionBtnId, label = suggestedWords[i])
          myTagList <- tagAppendChild(myTagList, mySuggestedWord)
        }
        myTagList
      } 
    }
  })
  
  output$ui4 <- renderUI({
    inputText <- getInputTextNoWhiteSpace()
    actualInputText <- getInputText()
    if (inputText=="")
      return()
    else if(hasTrailingWhiteSpace(actualInputText)){
      # Trigger Suggestion Algorithm
      suggestedWords <- suggestNextWord(corpus2DataFrame(inputText)$text, quadData, 10)
      # Set to local storage
      setSuggestionData(suggestedWords, "quad")
      numSuggestedWords <- length(suggestedWords)
      if(numSuggestedWords > 0){
        myTagList <- tagList()
        for(i in 1:numSuggestedWords){
          actionBtnId <- paste("action4_", i, sep="")
          mySuggestedWord <- actionButton(actionBtnId, label = suggestedWords[i])
          myTagList <- tagAppendChild(myTagList, mySuggestedWord)
        }
        myTagList
      } 
    }
  })
  
  observe({
    selectedWord <- getData()
    if(!is.null(selectedWord)){
      # append whitespace to the end of the inputText widget to trigger the next suggestion words
      updateTextInput(session, "text", 
                    value = paste(getInputTextNoWhiteSpace(), " ", selectedWord, " ", sep=""))
    }
  })
 
})