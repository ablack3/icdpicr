
downloadModuleInput <- function(id) {
  ns <- NS(id)

  tagList(
    textInput(ns("filename"), "Save output as", value = "data.csv"),
    downloadButton(ns("save"), "Save")
  )
}

downloadModule <- function(input, output, session, data) {
  output$save <- downloadHandler(
    filename = function() input$filename,
    content = function(file) {
         write.csv(data(), file, row.names = FALSE)
    }
  )
}