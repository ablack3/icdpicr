# ICDPIC app
icdpic <- function(){
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)

options(shiny.reactlog=T) # command f3 + right arrow

#setwd("C:\\Users\\awblack.MMCF\\Google Drive\\icdpic app")
setwd("/Users/adamblack/Google Drive/icdpic app")

source("./icdpicr/R/helper functions.R")
source("./icdpicr/R/trauma.R")
source("./icdpicr/R/altcharl.R")
source("./icdpicr/R/text.R")

source("./icdpicr/R/uploadModule.R")
source("./icdpicr/R/downloadModule.R")
source("./icdpicr/R/altcharlModule.R")
source("./icdpicr/R/traumaModule.R")

ntab_s1 <- read.csv("./lookup tables/ntab_s1.csv", stringsAsFactors = F, colClasses = c("character","character","integer","integer","character"))
etab_s1 <- read.csv("./lookup tables/etab_s1.csv", stringsAsFactors = F, colClasses = c("character","numeric","numeric","numeric"))

#sysdata<- load("./icdpicr/R/sysdata.rda")

#load_lookup_tables()



ui <- dashboardPage(
           
    dashboardHeader(title="ICDPIC-R"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Welcome", tabName = "welcome", icon = icon("home")),
            menuItem("Instructions", tabName = "instructions", icon = icon("list")),
            menuItem("Upload Data", tabName = "upload", icon = icon("level-up")),
            menuItem("Add Variables", tabName = "compute", icon = icon("cogs")),
            menuItem("Download Data", tabName = "download", icon = icon("level-down")),
            menuItem("Help", tabName = "help", icon = icon("question"))
        )
    ),
        
    dashboardBody(
        tabItems(
            tabItem(tabName = "welcome", box(width=12, welcome())),
            tabItem(tabName = "help", box(width=12, includeMarkdown("./Rmd/help_txt.Rmd"))),
            tabItem(tabName = "instructions", box(width=12, instructions())),
            tabItem(tabName = "upload",
                    fluidRow(
                        box(width=12,uploadModuleInput("datafile")),
                        box(width=12,div(style = 'overflow-x: scroll', dataTableOutput("input_table")))
                    )
            ),
            tabItem(tabName = "compute",
                    fluidRow(
                        box(width=12,
                            radioButtons("modules", "User Choices", 
                                     choices = c("ICD-9-CM Trauma"="trauma",
                                                 "Charleson Score\\Comorbidities (Alternate Version)"="altcharl"
                                                 ), 
                                     selected = NULL, inline = FALSE, width = NULL)
                        ),
                        box(width=12,
                            conditionalPanel('input.modules == "altcharl"', altcharlModuleInput("altcharl")),
                            conditionalPanel('input.modules == "trauma"', traumaModuleInput("trauma"))
                        )
                    )
            ),
            tabItem(tabName = "download",
                fluidRow(
                    box(width=12, downloadModuleInput("download")),
                    box(width=12, div(style = 'overflow-x: scroll', dataTableOutput("output_table")))
                )
            )
            
            
        )
    )
)



server <- function(input, output, session) {
   
    datafile <- callModule(uploadModule, "datafile")
  
    data_check1 <- reactive({ ifelse(nrow(datafile())>1, T, F) })
  
    altcharl_out <- callModule(altcharlModule, "altcharl", datafile)
    trauma_out <- callModule(traumaModule, "trauma", datafile)

    output$input_table <- renderDataTable({ datafile()})

    output_table <- reactive({
        print(input$modules)
        d <- datafile()
        if(input$modules == "trauma") d <- trauma_out()
        if(input$modules == "altcharl") d <- altcharl_out()
        d
    })
  
    observe({print(output_table())})
    callModule(downloadModule, "download", output_table)
  
    # outputs 
    output$output_table <- renderDataTable({ output_table() })
    output$data_check1_out <- renderText({ paste("data_check1:", data_check1()) })
    output$modules_out <- renderPrint({ input$modules })
    
    
    outputOptions(output, "output_table", suspendWhenHidden=FALSE)
  
}

shinyApp(ui, server)
}
