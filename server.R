source('engine.R')







# Define server logic to read selected file ----
server <- function(input, output) {
  

  output$df_340 <- renderDataTable({
    req(input$fluorescence)
    req(input$c340)
    df_340_ready <- reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, sheet_n = '340')
    
    }) # level 1 - output$df_340
  
  output$df_380 <- renderDataTable({    
    req(input$fluorescence)
    req(input$c380)
    df_380_ready <- reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, sheet_n = '380')
  }) # level 1 - output$df_380
  
  output$df_ratio <- renderDataTable({    
    req(input$fluorescence)
    req(input$cRatio)
    reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, sheet_n = 'Ratio')
    }) # level 1 - output$df_ratio
  
  output$df_custom_ratio <- renderDataTable({  
    req(input$fluorescence)
    req(df_340_ready)
    req(df_380_ready)
    custom_ratio(df_340_ready(), df_380_ready())
    }) # level 1 - output$df_380
  
  output$demo_verbatim <- renderText({
    (input$correct_time%%2) == 1
  })
  

  
  
  
} # level 0