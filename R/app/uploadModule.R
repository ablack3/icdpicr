# Exercise 4 - solution

uploadModuleInput <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), "Select a csv file", accept = c("text/csv")),
    textInput(ns("na.string"), "NA symbol", value = "NA")
  )
}

uploadModule <- function(input, output, session, ...) {

  userFile <- reactive({
    # If no file is selected, don't do anything
    req(input$file)
  })

  # The user's data, parsed into a data frame
  reactive({
    df <- read.csv(userFile()$datapath, header = T, stringsAsFactors = F, na.string = input$na.string, ...)
    
  })
}