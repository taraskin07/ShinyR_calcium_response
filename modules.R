
# Preliminary analysis ----------------------------------------------------


# Module UI function
sheetsDataUI <- function(id, name) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    selectInput(ns('sheets'), 
                paste0('Select the sheet for ', name),
                '', selected = '', multiple = FALSE)
    )
}




# Module server function
sheetsDataServer <- function(id, pattern) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$dataTS, message = FALSE))
        input$dataTS
      })
      
      
      
      # Getting the list of sheets in excel file
      sheets_in_the_file <-  eventReactive(eventExpr = {userFile()},
                                         valueExpr ={
                                           excel_sheets(userFile()$datapath)
                                         })
      
      # Creating SelectInput list with values = sheets
      observeEvent(userFile(),{
        updateSelectInput(inputId = ns("sheets"),
                          choices = sheets_in_the_file(),
                          selected = str_extract(sheets_in_the_file(), pattern)
        )
      })
      
      
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      return(reactive({input$sheets}))

    }
  )    
}