shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("       
              .shiny-input-container:not(.shiny-input-container-inline) {
                width: 100%;
              }

              .col-sm-8, .col-sm-3 {
                width: 100%;
              }
                    
      "))
    ),
  
  titlePanel("Predicting the next word"),
  
  mainPanel(
    div("This application predicts the next words when you enter a phrase."),
    div("Trigger the prediction when the phrase ends with a [SPACE]"),
    
    # Copy the line below to make a text input box
    textInput("text", label="", value = ""),

    #actionButton("predict", label = "Predict"),
    
    fluidRow(column(3, wellPanel(
      h5("Next Suggestion"),
      p("4-Grams Suggestion"),
      # This outputs the dynamic UI component
      uiOutput("ui4"),
      p(),
      p("3-Grams Suggestion"),
      # This outputs the dynamic UI component
      uiOutput("ui3"),
      p(),
      p("2-Grams Suggestion"),
      # This outputs the dynamic UI component
      uiOutput("ui2"),
      p()
    ))),

    hr(),
    
    fluidRow(column(3, wellPanel(
      h5("Result"),
      # This outputs the dynamic UI component
      verbatimTextOutput("valueText")
    )))
  )
  
))