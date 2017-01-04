
altcharlModuleInput <- function(id) {
    ns <- NS(id)
    
    tagList(
        radioButtons(ns("switch"), "Code Format", choices = c("no decimal"=1, "decimal"=2)),
        textOutput(ns("switch_out")),br(),br(),
        textInput(ns("dx_pre"),label="ICD9 diagnosis code prefix"),
        textOutput(ns("dx_cnt_out")), br(), br(),
        textInput(ns("px_pre"),label="ICD9 procedure code prefix"),
        textOutput(ns("px_cnt_out")), br(),br(),
        actionButton(ns("runButton"), "Run"),
        textOutput(ns("runButton_out"))
    )
}

altcharlModule <- function(input, output, session, datafile) {
    # count dx and px codes in data
    dx_cnt <- reactive({count_pre(datafile(),input$dx_pre)})
    px_cnt <- reactive({count_pre(datafile(), input$px_pre)})
    
    output$dx_cnt_out <- renderText({ paste(dx_cnt(),"diagnosis codes found")})
    output$px_cnt_out <- renderText({paste(px_cnt(),"procedure codes found")})
    output$runButton_out <- renderText({input$runButton})
    output$switch_out <- renderText(input$switch)
    

    
    eventReactive(input$runButton,{
        print("Running...")
        altcharl(datafile(), input$dx_pre, input$px_pre, input$switch)
    })
}
            

