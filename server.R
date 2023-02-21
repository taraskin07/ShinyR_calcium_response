library(shiny)
source("engine.R")
library(readxl)






# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$df_340 <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$fluorescence)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read_excel(input$fluorescence$datapath,
                         sheet='340')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}