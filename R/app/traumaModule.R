
traumaModuleInput <- function(id) {
    ns <- NS(id)
    
    tagList(
        radioButtons(ns("calc_method"), "ISS Calculation Method", choices = c("Any AIS = 6; ISS = 75"=1, "All AIS = 6 -> AIS = 5; ISS calculated normally"=2)),
        textOutput(ns("switch_out")),br(),br(),
        textInput(ns("dx_pre"),label="ICD9 diagnosis code prefix"),
        textOutput(ns("dx_cnt_out")), br(), br(),
        actionButton(ns("runButton"), "Run"),
        textOutput(ns("runButton_out")),
        textOutput(ns("log"))
    )
}

traumaModule <- function(input, output, session, datafile) {
      # count dx and px codes in data
      regex_dx <- reactive({paste(input$dx_pre,"([0-9]+)",sep="")})
      dx_colnames <- reactive({grep(regex_dx(), names(datafile()), value = T)})
      dx_cnt <- reactive({ ifelse(input$dx_pre == "",0,length(dx_colnames()))})
    
    output$dx_cnt_out <- renderText({ paste(dx_cnt(),"diagnosis codes found")})
    output$runButton_out <- renderText({ifelse(input$runButton>0,"Proceed to next step","")})
    output$switch_out <- renderText(input$switch2)
  
    eventReactive(input$runButton,{
        print("Running...")
        #ifelse(dx_cnt()>0, trauma(datafile(), input$dx_pre, input$switch1, input$switch2),datafile())
        trauma(datafile(), input$dx_pre, input$calc_method)
    })
}


