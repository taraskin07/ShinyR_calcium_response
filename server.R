source('engine.R')







# Define server logic to read selected file ----
server <- function(input, output) {
  

# Preliminary analysis/ 1st box -------------------------------------------

  # All 4 tables rendering
  df_340_ready <- eventReactive(eventExpr = {input$fluorescence 
                                             input$disp
                                             input$correct_time
                                             input$change_names
                                             input$c340},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$c340)
                                  reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = '340')
                                }) #level 1 - df_340_ready
  
  output$df_340 <- DT::renderDataTable({
    req(input$fluorescence)
    req(input$c340)
    df_340_ready()
                                  }) # level 1 - output$df_340
    
    
  
  df_380_ready <- eventReactive(eventExpr = {input$fluorescence 
                                             input$disp
                                             input$correct_time
                                             input$change_names
                                             input$c380},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$c380)
                                  reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = '380')
                                            }) # level 1 - df_380_ready
  
  output$df_380 <- DT::renderDataTable({    
    req(input$fluorescence)
    req(input$c380)
    df_380_ready()
 }) # level 1 - output$df_380
  
  
  
  df_ratio_ready <- eventReactive(eventExpr = {input$fluorescence 
                                               input$disp
                                               input$correct_time
                                               input$change_names
                                               input$cRatio},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$cRatio)
                                  reading_xls(input$fluorescence, input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = 'Ratio')
    }) # level 1 - df_ratio_ready
  
  output$df_ratio <- DT::renderDataTable({
    req(input$fluorescence)
    req(input$cRatio)
    df_ratio_ready()}) # level 1 - output$df_ratio

  
  df_custom_ratio_ready <- eventReactive(eventExpr = {input$fluorescence 
                                                      input$disp
                                                      input$correct_time
                                                      input$change_names
                                                      input$c340
                                                      input$c380},
                                          valueExpr = {
                                            req(input$fluorescence)
                                            req(input$c340)
                                            req(input$c380)
                                            custom_ratio(df_340_ready(), df_380_ready())}) # level 1 - df_custom_ratio_ready

  output$df_custom_ratio <- DT::renderDataTable({  
    req(input$fluorescence)
    req(df_340_ready)
    req(df_380_ready)
    df_custom_ratio_ready()
    }) # level 1 - output$df_custom_ratio
  
  
  # Save as excel file
  output$SaveXlsBox1 <- downloadHandler(
    filename = function() { "ProcessedTable.xlsx"},
    content = function(file) {write_xlsx(list('340'=df_340_ready(), '380'=df_380_ready(), 'ratio' = df_ratio_ready(), 'custom_ratio' = df_custom_ratio_ready()), path = file)}
  )
  
  
  # # Additional box
  # output$demo_verbatim <- renderText({
  #   (input$correct_time%%2) == 1
  # })
  
# Preliminary analysis/ 2d box -------------------------------------------  
  
  df_340_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$c340)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = '340') 
                                       
                                       }) # # eventReactive / df_340_basic_stat
  
  output$df_340_basic_stat_out <- DT::renderDataTable({
    basic_statistics(df_340_basic_stat())

  }) # output$df_340_basic_stat
  
  
  
  df_380_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$c380)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = '380') 
                                       
                                     }) # # eventReactive / df_380_basic_stat
  
  output$df_380_basic_stat_out <- DT::renderDataTable({
    basic_statistics(df_380_basic_stat())

    
  }) # output$df_380_basic_stat  
  
  
  
  df_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$cRatio)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = 'Ratio') 
                                       
                                     }) # # eventReactive / df_ratio_basic_stat
  
  output$df_ratio_basic_stat_out <- DT::renderDataTable({
    basic_statistics(df_ratio_basic_stat())

  }) # output$df_ratio_basic_stat_out  
  
  
  
  df_custom_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                      valueExpr = {
                                        req(input$fluorescence)
                                        req(input$c340)
                                        req(input$c380)
                                        custom_ratio(df_340_basic_stat(), df_380_basic_stat())
                                      }) # # eventReactive / df_ratio_basic_stat                                    
  
  output$df_custom_ratio_basic_stat_out <- DT::renderDataTable({
    basic_statistics(df_custom_ratio_basic_stat())

  }) # output$df_ratio_basic_stat_out  
  
  

# Preliminary analysis/ 3d box, plots------------------------------------------------

    observeEvent(input$plot_all, {
      output$plot340 <- renderPlotly({
        req(input$plot_all, df_340_basic_stat())
        ggplotly_render(df_340_basic_stat())})
      
      
      output$plot380 <- renderPlotly({
        req(input$plot_all, df_380_basic_stat())
        ggplotly_render(df_380_basic_stat())})
      
      
      output$plot_ratio <- renderPlotly({
        req(input$plot_all, df_ratio_basic_stat())
        ggplotly_render(df_ratio_basic_stat())})
      
      
      output$plot_custom_ratio <- renderPlotly({
        req(input$plot_all, df_custom_ratio_basic_stat())
        ggplotly_render(df_custom_ratio_basic_stat())
      })
    }) # /level 1, observeEvent input$plot_single
  
    observeEvent(input$plot_single, {
      
      output$plot340 <- renderPlotly({
        req(input$cell_to_plot, input$cellName, df_340_basic_stat())
        ggplotly_render(get_col_names(df_340_basic_stat(), input$cellName, input$cell_to_plot))})
      
      
      
      output$plot380 <- renderPlotly({
        req(input$cell_to_plot, input$cellName, df_380_basic_stat())
        ggplotly_render(get_col_names(df_380_basic_stat(), input$cellName, input$cell_to_plot))})

      
      
      
      output$plot_ratio <- renderPlotly({
        req(input$cell_to_plot, input$cellName, df_ratio_basic_stat())
        ggplotly_render(get_col_names(df_ratio_basic_stat(), input$cellName, input$cell_to_plot))
      })
      
      
      
      output$plot_custom_ratio <- renderPlotly({
      req(input$cell_to_plot, input$cellName, df_custom_ratio_basic_stat())
      ggplotly_render(get_col_names(df_custom_ratio_basic_stat(), input$cellName, input$cell_to_plot))
    })
  
  
    }) # /level 1, observeEvent input$plot_single

    
    # Now creating reactive values list of cells to include
    
    rmcellValues <- reactiveValues()
    
    
    # Buttons to Exclude/Undo/Include/Reset cells 
    
    observeEvent(input$exclude_cell, {
      rmcellValues$cList <- unique(c(isolate(rmcellValues$cList), isolate(paste0(input$cellName, input$cell_to_plot))))
      
      
      output$list_of_cells<-renderPrint({
        rmcellValues$cList
      })
      }) # /level 1, observeEvent input$exclude_cell
    
    observeEvent(input$exclude_reset, {
      rmcellValues$cList <- c()
      
      
      output$list_of_cells<-renderPrint({
        rmcellValues$cList
      })
    }) # /level 1, observeEvent input$exclude_reset
    
    observeEvent(input$exclude_undo, {
      req(rmcellValues$cList)
      rmcellValues$cList <- rmcellValues$cList[-length(rmcellValues$cList)]
      
      
      output$list_of_cells<-renderPrint({
        rmcellValues$cList
      })
    }) # /level 1, observeEvent input$exclude_undo
    
    observeEvent(input$include_cell, {
      req(rmcellValues$cList)
      rmcellValues$cList <- rmcellValues$cList[!rmcellValues$cList == isolate(paste0(input$cellName, input$cell_to_plot))]
      
      
      output$list_of_cells<-renderPrint({
        rmcellValues$cList
      })
    }) # /level 1, observeEvent input$exclude_undo
    
    
    # Button to obtain new dataframes without bad cells information
    

      
      df_340_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 

                                                    valueExpr = {
                                                      
                                                    req(input$fluorescence)
                                                    req(input$c340)
                                                    subset(df_340_ready(), select = colnames(df_340_ready()) != rmcellValues$cList)
                                                    
                                                    }
                                       ) # # eventReactive / df_340_excluded
      

    
    
    observeEvent(input$plot_new_all, {
      output$plot340 <- renderPlotly({
        req(input$plot_new_all, df_340_excluded())
        ggplotly_render(df_340_excluded())})
    })

    
  
  
  

} # level 0