library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Treadrightly: Predictive Text"),
  
  sidebarPanel(
    textInput("inputText", "Input text")
  ),
  
  mainPanel(
    h3(textOutput("outputText"))
  )
  
))