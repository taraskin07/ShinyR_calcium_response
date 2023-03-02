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
    })
  
    observeEvent(input$plot_single, {
      output$plot340 <- renderPlotly({
        req(input$cell_to_plot, df_340_basic_stat())
        colm <- paste0('cell-', input$cell_to_plot)
        ggplotly_render(df_340_basic_stat()[c('Time', colm)])})
    })
  
  
  # vals <- reactiveValues(
  #   keepcolumns = rep(TRUE, nrow(df_340_ready()))
  # )
  # 
  # output$plot340 <- renderPlot({
  #   # Plot the kept and excluded points as two separate data sets
  #   df <- df_340_ready()
  #   keep    <- df[ ,vals$keepcolumns, drop = FALSE]
  #   exclude <- df[ ,!vals$keepcolumns, drop = FALSE]
  # 
  #   df_tidy_keep <- keep %>%
  #     pivot_longer(!Time, names_to = "cells", values_to = "r.u.")
  #   p <- ggplot(df_tidy_keep, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+ geom_point(size = 0.2) + theme(legend.position = "none")
  #   ggplotly(p)
  # })
  # 
  # # Toggle points that are clicked
  # observeEvent(input$plot340_click, {
  #   res <- nearPoints(df_340_ready(), input$plot340_click, allRows = TRUE)
  # 
  #   vals$keepcolumns <- xor(vals$keepcolumns, res$selected_)
  # })
  # 
  # # Toggle points that are brushed, when button is clicked
  # observeEvent(input$exclude_toggle, {
  #   res <- brushedPoints(df_340_ready(), input$plot340_brush, allRows = TRUE)
  # 
  #   vals$keepcolumns <- xor(vals$keepcolumns, res$selected_)
  # })
  # 
  # # Reset all points
  # observeEvent(input$exclude_reset, {
  #   vals$keepcolumns <- rep(TRUE, nrow(df_340_ready()))
  # })


  
  
  
} # level 0