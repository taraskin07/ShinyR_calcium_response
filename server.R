source('engine.R')







# Define server logic to read selected file ----
server <- function(input, output) {

  

# Preliminary analysis -----------------------------------------------------    

  
  
# Preliminary analysis/ 1st box -------------------------------------------

  # All 4 tables rendering
  df_340_ready <- eventReactive(eventExpr = {input$fluorescence 
                                             input$disp
                                             input$correct_time
                                             input$change_names
                                             input$change_names_button
                                             input$c340},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$c340)
                                  reading_xls(input$fluorescence, disp_opt=input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = '340')
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
                                             input$change_names_button
                                             input$c380},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$c380)
                                  reading_xls(input$fluorescence, disp_opt=input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = '380')
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
                                               input$change_names_button
                                               input$cRatio},
                                valueExpr = {
                                  req(input$fluorescence)
                                  req(input$cRatio)
                                  reading_xls(input$fluorescence, disp_opt=input$disp, input$correct_time, input$change_names, input$cellName, sheet_n = 'Ratio')
    }) # level 1 - df_ratio_ready
  
  output$df_ratio <- DT::renderDataTable({
    req(input$fluorescence)
    req(input$cRatio)
    df_ratio_ready()}) # level 1 - output$df_ratio

  
  df_custom_ratio_ready <- eventReactive(eventExpr = {input$fluorescence 
                                                      input$disp
                                                      input$correct_time
                                                      input$change_names
                                                      input$change_names_button
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
    filename = function() { filename(input$fluorescence, "ProcessedTable.xlsx")},
    content = function(file) {write_xlsx(list('340'=df_340_ready(), '380'=df_380_ready(), 'ratio' = df_ratio_ready(), 'custom_ratio' = df_custom_ratio_ready()), path = file)}
  )
  
  
  
  
# Preliminary analysis/ 2d box - STATISTICS -------------------------------------------  
  
  df_340_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$c340)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = '340') 
                                       
                                       }) # # eventReactive / df_340_basic_stat
  
  df_340_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                           
                                           valueExpr = {
                                             req(input$fluorescence)
                                             req(input$c340)
                                             return(basic_statistics(df_340_basic_stat()))
                                             
                                           }) # # eventReactive / df_340_basic_stat_table
  
  output$df_340_basic_stat_out <- DT::renderDataTable({
    df_340_basic_stat_table()

  }) # output$df_340_basic_stat
  
  
  
  df_380_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$c380)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = '380') 
                                       
                                     }) # # eventReactive / df_380_basic_stat
  
  
  df_380_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                           
                                           valueExpr = {
                                             req(input$fluorescence)
                                             req(input$c380)
                                             return(basic_statistics(df_380_basic_stat()))
                                             
                                           }) # # eventReactive / df_380_basic_stat_table
  
  output$df_380_basic_stat_out <- DT::renderDataTable({
    df_380_basic_stat_table()

    
  }) # output$df_380_basic_stat  
  
  
  
  df_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$fluorescence)
                                       req(input$cRatio)
                                       reading_xls(input$fluorescence, disp_opt="all", input$correct_time, input$change_names, input$cellName, sheet_n = 'Ratio') 
                                       
                                     }) # # eventReactive / df_ratio_basic_stat
  
  df_ratio_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                           
                                           valueExpr = {
                                             req(input$fluorescence)
                                             req(input$cRatio)
                                             return(basic_statistics(df_ratio_basic_stat()))
                                             
                                           }) # # eventReactive / df_ratio_basic_stat_table
  
  output$df_ratio_basic_stat_out <- DT::renderDataTable({
    df_ratio_basic_stat_table()

  }) # output$df_ratio_basic_stat_out  
  
  
  
  df_custom_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                      valueExpr = {
                                        req(input$fluorescence)
                                        req(input$c340)
                                        req(input$c380)
                                        custom_ratio(df_340_basic_stat(), df_380_basic_stat())
                                      }) # # eventReactive / df_ratio_basic_stat  
  
  df_custom_ratio_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                             
                                             valueExpr = {
                                               req(input$fluorescence)
                                               req(input$c340)
                                               req(input$c380)
                                               return(basic_statistics(df_custom_ratio_basic_stat()))
                                               
                                             }) # # eventReactive / df_ratio_basic_stat_table
  
  output$df_custom_ratio_basic_stat_out <- DT::renderDataTable({
    df_custom_ratio_basic_stat_table()

  }) # output$df_ratio_basic_stat_out  
  
  # Save BASIC STATISTICS as excel file
  output$SaveXlsBoxStat <- downloadHandler(
    filename = function() {filename(input$fluorescence, "BasicStatisticsTable.xlsx")},
    content = function(file) {write_xlsx(list('340'=cell_number_row(df_340_basic_stat_table()), '380'=cell_number_row(df_380_basic_stat_table()), 'ratio' = cell_number_row(df_ratio_basic_stat_table()), 'custom_ratio' = cell_number_row(df_custom_ratio_basic_stat_table())), path = file)}
    )

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
        req(input$cell_to_plot, df_340_basic_stat())
        ggplotly_render(get_col_names(df_340_basic_stat(), input$cellName, input$cell_to_plot, format = input$change_names))})
      
      
      
      output$plot380 <- renderPlotly({
        req(input$cell_to_plot, df_380_basic_stat())
        ggplotly_render(get_col_names(df_380_basic_stat(), input$cellName, input$cell_to_plot, format = input$change_names))})

      
      
      
      output$plot_ratio <- renderPlotly({
        req(input$cell_to_plot, df_ratio_basic_stat())
        ggplotly_render(get_col_names(df_ratio_basic_stat(), input$cellName, input$cell_to_plot, format = input$change_names))
      })
      
      
      
      output$plot_custom_ratio <- renderPlotly({
      req(input$cell_to_plot, df_custom_ratio_basic_stat())
      ggplotly_render(get_col_names(df_custom_ratio_basic_stat(), input$cellName, input$cell_to_plot, format = input$change_names))
    })
  
  
    }) # /level 1, observeEvent input$plot_single

    
    # Now creating reactive values list of cells to include
    
    rmcellValues <- reactiveValues()
      
    
    # Buttons to Exclude/Undo/Include/Reset cells 
    
    observeEvent(input$exclude_cell, {
      
      
      rmcellValues$cList <- unique(c(isolate(rmcellValues$cList), isolate(constructing_names(input$cellName, input$cell_to_plot, format = input$change_names))))
      
      
      output$list_of_cells<-renderPrint({
        gtools::mixedsort(rmcellValues$cList, decreasing = T)
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
        gtools::mixedsort(rmcellValues$cList, decreasing = T)
      })
    }) # /level 1, observeEvent input$exclude_undo
    
    observeEvent(input$include_cell, {
      req(rmcellValues$cList)
      rmcellValues$cList <- rmcellValues$cList[!rmcellValues$cList == isolate(constructing_names(input$cellName, input$cell_to_plot, format = input$change_names))]
      
      
      output$list_of_cells<-renderPrint({
        gtools::mixedsort(rmcellValues$cList, decreasing = T)
      })
    }) # /level 1, observeEvent input$include_cell
    
    
    

# Excluding cells ---------------------------------------------------------

    
    # Button to obtain new dataframes without bad cells information
    
      
      df_340_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 

                                                    valueExpr = {
                                                      
                                                    req(input$fluorescence)
                                                    req(input$c340)                  
                                                    subset(df_340_basic_stat(), select = !(colnames(df_340_basic_stat()) %in% rmcellValues$cList))
                                                    
                                                    }
                                       ) # # eventReactive / df_340_excluded
    
    
      df_380_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                       
                                       valueExpr = {
                                         
                                         req(input$fluorescence)
                                         req(input$c380)                  
                                         subset(df_380_basic_stat(), select = !(colnames(df_380_basic_stat()) %in% rmcellValues$cList))
                                       
                                                    }
                                      ) # # eventReactive / df_380_excluded
      
      
      df_ratio_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                       
                                       valueExpr = {
                                         
                                         req(input$fluorescence)
                                         req(input$cRatio)                  
                                         subset(df_ratio_basic_stat(), select = !(colnames(df_ratio_basic_stat()) %in% rmcellValues$cList))
                                         
                                       }
      ) # # eventReactive / df_ratio_excluded
      
      
      df_custom_ratio_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                         
                                         valueExpr = {
                                           
                                           req(input$fluorescence)
                                           req(input$c340)   
                                           req(input$c380)
                                           subset(df_custom_ratio_basic_stat(), select = !(colnames(df_custom_ratio_basic_stat()) %in% rmcellValues$cList))
                                           
                                         }
      ) # # eventReactive / df_custom_ratio_excluded

    
    
    observeEvent(input$plot_new_all, {
      
      output$plot340 <- renderPlotly({
        req(input$plot_new_all, df_340_excluded())
        ggplotly_render(df_340_excluded())})
    
      output$plot380 <- renderPlotly({
        req(input$plot_new_all, df_380_excluded())
        ggplotly_render(df_380_excluded())})
      
      output$plot_ratio <- renderPlotly({
        req(input$plot_new_all, df_ratio_excluded())
        ggplotly_render(df_ratio_excluded())})
      
      output$plot_custom_ratio <- renderPlotly({
        req(input$plot_new_all, df_custom_ratio_excluded())
        ggplotly_render(df_custom_ratio_excluded())})
      
      }) # observeEvent / input$plot_new_all

    
    
      # Save DATA WITHOUT BAD CELLS as excel file
    
      output$SaveXlsBoxNoBadCells <- downloadHandler(
        filename = function() {filename(input$fluorescence, "CleanTable.xlsx")},
        content = function(file) {write_xlsx(list('340'=df_340_excluded(), '380'=df_380_excluded(), 'ratio' = df_ratio_excluded(), 'custom_ratio' = df_custom_ratio_excluded(), 'excluded_cells' = data.frame(Excluded_cells=gtools::mixedsort(rmcellValues$cList, decreasing = T))), path = file)}
      )
    

# Debugging section -------------------------------------------------------

    output$df_340_ready_db <- DT::renderDataTable({
      req(input$fluorescence)
      req(input$c340)
      req(input$new_dataframes)
      df_340_ready()
      }) 
    
    
    output$df_340_excluded_db <- DT::renderDataTable({
      req(input$fluorescence)
      req(input$c340)
      req(input$new_dataframes)
      df_340_excluded()
    })
    
    
    output$rmcellValues_cList <- renderPrint({
      rmcellValues$cList
    })
    
    

# Analyzing amplitude -----------------------------------------------------    
    
    
    # All 4 tables of CLEAN DATA rendering
    

# 340

    df_340_clean <- eventReactive(eventExpr = {input$clean_file
      input$cl340},
      valueExpr = {
        req(input$clean_file)
        req(input$cl340)
        read_excel(input$clean_file$datapath,
                   sheet='340')
        
                  }
                                  ) #level 1 - df_340_clean
    
    output$cl_340 <- DT::renderDataTable({
      req(input$clean_file)
      req(input$cl340)
      df_340_clean()
                                            }) # level 1 - output$cl_340
    

# 380 

   
    df_380_clean <- eventReactive(eventExpr = {input$clean_file
      input$cl380},
      valueExpr = {
        req(input$clean_file)
        req(input$cl380)
        read_excel(input$clean_file$datapath,
                   sheet='380')
        
      }
    ) #level 1 - df_380_clean
    
    
    output$cl_380 <- DT::renderDataTable({
      req(input$clean_file)
      req(input$cl380)
      df_380_clean()
    }) # level 1 - output$cl_380
    
    

# Ratio 
    
    
    df_ratio_clean <- eventReactive(eventExpr = {input$clean_file
      input$clRatio},
      valueExpr = {
        req(input$clean_file)
        req(input$clRatio)
        read_excel(input$clean_file$datapath,
                   sheet='ratio')
        
      }
    ) #level 1 - df_ratio_clean
    
    
    output$cl_ratio <- DT::renderDataTable({
      req(input$clean_file)
      req(input$clRatio)
      df_ratio_clean()
    }) # level 1 - output$cl_ratio


    
# Custom Ratio 
    
    
    df_custom_ratio_clean <- eventReactive(eventExpr = {input$clean_file},
      valueExpr = {
        req(input$clean_file)
        read_excel(input$clean_file$datapath,
                   sheet='custom_ratio')
        
      }
    ) #level 1 - df_custom_ratio_clean
    
    
    output$cl_custom_ratio <- DT::renderDataTable({
      req(input$clean_file)
      req(input$cl340)
      req(input$cl380)
      df_custom_ratio_clean()
    }) # level 1 - output$cl_custom_ratio    
   
    
# Excluded cells    
    df_excluded_cells_list <- eventReactive(eventExpr = {input$clean_file},
                                            valueExpr = {
                                              req(input$clean_file)
                                              read_excel(input$clean_file$datapath,
                                                         sheet='excluded_cells')
                                              }) # level 1 - df_excluded_cells_list  
    
    # Calculating amplitudes
    
# 340
    
    df_340_amplitude <- eventReactive(eventExpr = {input$amplitudeStat
      input$cl340},
      valueExpr = {
        req(input$clean_file)
        req(input$cl340)
        find_amplitude(df_340_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_340_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$cl340)
      df_340_amplitude()
    })
    
    
# Ratio
    
    df_ratio_amplitude <- eventReactive(eventExpr = {input$amplitudeStat
      input$clRatio},
      valueExpr = {
        req(input$clean_file)
        req(input$clRatio)
        find_amplitude(df_ratio_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_ratio_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$clRatio)
      df_ratio_amplitude()
    })
    
    
# Custom Ratio
    
    df_custom_ratio_amplitude <- eventReactive(eventExpr = {input$amplitudeStat},
      valueExpr = {
        req(input$clean_file)
        find_amplitude(df_custom_ratio_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_custom_ratio_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$cl340)
      req(input$cl380)
      df_custom_ratio_amplitude()
    })
    
    
    
    # IN THIS CASE WE NEED TO FIND MINIMUM INSTEAD! 
# 380
    
    df_380_amplitude <- eventReactive(eventExpr = {input$amplitudeStat
      input$cl380},
      valueExpr = {
        req(input$clean_file)
        req(input$cl380)
        find_amplitude_380(df_380_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_380_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$cl380)
      df_380_amplitude()
    })    
    
    
# Summary for amplitudes
    
    # 340
    df_340_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_340_amplitude(), df_excluded_cells_list())
                                    })
    
    # 380
    df_380_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_380_amplitude(), df_excluded_cells_list())
                                    })
    
    # Ratio
    df_ratio_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_ratio_amplitude(), df_excluded_cells_list())
                                    })
    
    # Custom Ratio
    df_custom_ratio_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_custom_ratio_amplitude(), df_excluded_cells_list())
                                    })
    
    
# Save DATA ANALIZING AMPLITUDES as excel file
    
    output$SaveXlsAmpl <- downloadHandler(
      filename = function() {filename(input$fluorescence, "Amplitudes.xlsx")},
      content = function(file) {write_xlsx(list('340'=df_340_amplitude(), 
                                                '380'=df_380_amplitude(), 
                                                'ratio' = df_ratio_amplitude(), 
                                                'custom_ratio' = df_custom_ratio_amplitude(),
                                                '340_summary' = df_340_summary(),
                                                '380_summary' = df_380_summary(),
                                                'ratio_summary' = df_ratio_summary(),
                                                'custom_ratio_summary' = df_custom_ratio_summary()
                                                ), path = file)}
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
} # level 0