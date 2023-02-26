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
  
  
  
  
} # level 0