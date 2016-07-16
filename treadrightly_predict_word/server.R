library(shiny)

cleanInput = function(input)
{
  input = tolower(input)
  input = stripWhitespace(input)
  input = removeNumbers(input)
  input = removePunctuation(input)
  #input = removeWords(input, stopwords("SMART"))
  #input = stemDocument(input)
  input = strsplit(input, " ")[[1]]
  input = input[input != ""]
  return (input)
}

predictWord = function(input)
{
  pred = character()
  input = cleanInput(input)
  temp = unlist(strsplit(input, " ", fixed=TRUE))
  if (length(temp) > 2)
  {
    temp = temp[(length(temp) - 1):length(temp)]
    len = length(temp)
    searchstr = paste("^", temp[(len-1)], " ", temp[len], " ", sep="")
    searchres = head(wc3[grepl(searchstr, wc3$ngram), ], 1)
    if (length(searchres$ngram) > 0)
    {
      pred = unlist(strsplit(as.character(searchres$ngram[1]), " ", fixed=TRUE))[3]
      return (pred)
    }
  }
  if (length(temp) == 2)
  {
    temp = temp[2]
    searchstr = paste("^", temp, " ", sep="")
    searchres = head(wc2[grepl(searchstr, wc2$ngram), ], 1)
    if (length(searchres$ngram) > 0)
    {
      pred = unlist(strsplit(as.character(searchres$ngram[1]), " ", fixed=TRUE))[2]
      return (pred)
    }
  }
  searchres = head(wc1, 1)
  if (length(searchres$ngram) > 0)
  {
    pred = as.character(searchres$ngram[1])
    return (pred)
  }
}

library(tm)
wc1 = readRDS("wc1.RDS")
wc2 = readRDS("wc2.RDS")
wc3 = readRDS("wc3.RDS")

shinyServer(function(input, output) {
  
  output$outputText <- renderText({
    outputText <- predictWord(input$inputText)
  })
  
})