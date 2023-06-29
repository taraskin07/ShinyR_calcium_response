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
    }) # /level 1, observeEvent input$plot_all
  
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


    
# Excluding cells ---------------------------------------------------------
    
    
# Button to obtain new dataframes without bad cells information    
    
    
    # Now creating reactive values list of cells to exclude
    
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
    
    
    

# Shifting curves / box 1 ---------------------------------------------------------

    # Getting the list of sheets in excel file
    sheets_in_the_file = eventReactive(eventExpr = {input$read_sheets},
                                       valueExpr ={
        excel_sheets(input$read_sheets$datapath)
    })
    
    # Creating SelectInput list with values = sheets
    observeEvent(input$read_sheets,{
      updateSelectInput(inputId = "sheets",
                        choices = sheets_in_the_file(),
                        selected = str_extract(sheets_in_the_file(), '^[Rr]atio$')
                        )
            })
    


    
    # Rendering datatable related to selected sheet
    dt_to_shift <- eventReactive(eventExpr = {input$read_sheets
                                              input$sheets},
                  valueExpr = {read_excel(input$read_sheets$datapath, sheet = input$sheets)})
    
    
    output$dt_to_shift_out <- DT::renderDataTable({
      req(input$read_sheets)
      req(input$sheets)
      dt_to_shift()
    }) 
 
 
# Shifting curves / box 2 -------------------------------------------------
    
    
    # Rendering initial plot single
    observeEvent(input$plots_init_single, {
      output$plot_shift_upper <- renderPlotly({
        req(input$sheets,
            input$cell_to_plot_shift,
            input$min_t_shift,
            input$max_t_shift,
            input$start_t_shift,
            input$end_t_shift)
        ggplotly_render(single_plot(dt_to_shift(), 
                                    input$cell_to_plot_shift), 
                                    baseline = T, 
                                    b_min = input$min_t_shift, 
                                    b_max = input$max_t_shift, 
                                    region = T, 
                                    r_min = input$start_t_shift, 
                                    r_max = input$end_t_shift)
        })
    }) # /level 1, observeEvent input$plots_init_single
    
    
    # Rendering initial plot all
    observeEvent(input$plots_init_all, {
      output$plot_shift_upper <- renderPlotly({
        req(input$sheets)
        ggplotly_render(dt_to_shift(), 
                        baseline = T, 
                        b_min = input$min_t_shift, 
                        b_max = input$max_t_shift, 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)})
    }) # /level 1, observeEvent input$plots_init_all
    
    
    # Shifting curves
    
    # Lag values datatable
    
    lag_values_df <- eventReactive(eventExpr = {input$shift_curves}, valueExpr = {
      
      req(input$read_sheets)
      req(input$sheets)
      shifted_main_cell_values <- finding_shifted_curve(dt_to_shift(), main_cell_number = input$cell_to_plot_shift, lower = input$start_t_shift, upper = input$end_t_shift, max_lag = input$max_lag)
      
      #Resulting dataframe
      lag_data <- data.frame(A = character(), B = numeric())
      colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])
      
      shifted_info <- shifting_curves_info(lag_data, dt_to_shift(), shifted_main_cell_values, lower = input$start_t_shift, upper = input$end_t_shift, max_lag = input$max_lag)
      return(shifted_info)
      })
    
    output$lag_values_df_out <- DT::renderDataTable({
      req(input$read_sheets)
      req(input$sheets)
      lag_values_df()
    }) 
    
    
    # Shifted datatable
    shifted_df <- eventReactive(eventExpr = {input$shift_curves
                                             input$plots_shift_omit}, valueExpr = {
      
      req(input$read_sheets)
      req(input$sheets)
      shifted_main_cell_values <- finding_shifted_curve(dt_to_shift(), main_cell_number = input$cell_to_plot_shift, lower = input$start_t_shift, upper = input$end_t_shift, max_lag = input$max_lag)
      shifted_result <- shifting_curves(dt_to_shift(), shifted_main_cell_values, lower = input$start_t_shift, upper = input$end_t_shift, max_lag = input$max_lag)
      
      if((input$plots_shift_omit[1]%%2) == 1) {
        shifted_result <- na.omit(shifted_result)
        return(shifted_result)
      } else {return(shifted_result)}
      
      
    }, ignoreNULL = FALSE)
    

    
    # Rendering lower plot
    observeEvent(input$plots_shift_single, {
      
      output$plot_shift_lower <- renderPlotly({
        req(input$sheets)
        ggplotly_render(single_plot(shifted_df(), input$cell_to_plot_shift), 
                        baseline = T, 
                        b_min = input$min_t_shift, 
                        b_max = input$max_t_shift, 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
        }) # output$plot_shift_lower
      
      
    }) # /level 1, observeEvent input$plots_shift_single
    
    observeEvent(input$plots_shift_all, {
      
      output$plot_shift_lower <- renderPlotly({
        req(input$sheets)
        ggplotly_render(shifted_df(), 
                        baseline = T, 
                        b_min = input$min_t_shift, 
                        b_max = input$max_t_shift, 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
        }) # output$plot_shift_lower

    
      
    }) # /level 1, observeEvent input$plots_shift_all
    
    
    
    # Save SHIFTED curves as excel file
    
    read_sheets_value <- reactive({
      
      if((input$plots_shift_omit[1]%%2) == 1) {
        return(TRUE)
      }
      
      })
    output$read_sheets_value_out <- renderPrint({read_sheets_value()})
    
    output$SavePltsShift <- downloadHandler(
      filename = function() {filename(input$read_sheets$name, "Shifted.xlsx")},
      content = function(file) {
        df_list <- list('current' = shifted_df(),
                        'lags' = lag_values_df())
        names(df_list) <- c(input$sheets, paste0(input$sheets, '_lags'))
        write_xlsx(df_list, path = file)
        }
    )
    

# Average line / Box 3 ----------------------------------------------------

    # Shifted Average

    shifted_average <- eventReactive(eventExpr = {input$plots_average_shifted}, valueExpr = {

      df_t <- time_col_name(shifted_df())
      
      df_t <- df_t %>% 
        add_column(Average = rowMeans(df_t[-grep('^Time$', colnames(df_t))]))
      
      return(df_t)
      
      
      }) # shifted_average / eventReactive


    # Rendering lower plot for shifted average
    observeEvent(input$plots_average_shifted, {
      
      output$plot_average_lower <- renderPlotly({
        req(shifted_df())
        
        
        ggplotly_render(shifted_average()[, c('Time', 'Average')], 
                        baseline = T, 
                        b_min = input$min_t_shift, 
                        b_max = input$max_t_shift, 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
        }) # output$plot_average_lower
      
      
    }) # /level 1, observeEvent input$plots_average_shifted
    
    
    # Initial Average
    
    init_average <- eventReactive(eventExpr = {input$plots_average_init}, valueExpr = {
      
      df_t <- time_col_name(dt_to_shift())
      
      df_t <- df_t %>% 
        add_column(Average = rowMeans(df_t[-grep('^Time$', colnames(df_t))]))
      
      return(df_t)
      
      
    }) # shifted_average / eventReactive
    
    
    # Rendering lower plot for shifted average
    observeEvent(input$plots_average_init, {
      
      output$plot_average_upper <- renderPlotly({
        req(dt_to_shift())
        
        
        ggplotly_render(init_average()[, c('Time', 'Average')], 
                        baseline = T, 
                        b_min = input$min_t_shift, 
                        b_max = input$max_t_shift, 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
      
        }) # output$plot_average_upper
      
      
    }) # /level 1, observeEvent input$plots_average_init
    
    
    
    # Saving data with Average column
    
    output$SaveAverage <- downloadHandler(
      filename = function() {filename(input$read_sheets$name, "Shifted_with_Average.xlsx")},
      content = function(file) {
        df_list <- list('first' = init_average(),
                        'second' = shifted_average())
        names(df_list) <- c(paste0(input$sheets, '_init'), paste0(input$sheets, '_shifted'))
        write_xlsx(df_list, path = file)
      }
    )
    
    
    

    
    
    
# Rotating plot / box 1 ---------------------------------------------------------
    
    
    # Getting the list of sheets in excel file
    data_sheets_in_file <-  eventReactive(eventExpr = {input$read_curves},
                                       valueExpr ={
                                         excel_sheets(input$read_curves$datapath)
                                       })
    
    
    # Creating SelectInput list with values = sheets
    observeEvent(input$read_curves,{
      updateSelectInput(inputId = "data_sheets",
                        choices = data_sheets_in_file(),
                        selected = str_extract(data_sheets_in_file(), '^[Rr]atio$')
      )
    })   
    
    # Later it will be used to save changes in dataframe after single correction
    dataframe_to_process <- reactiveVal(NULL)
    
    # Rendering datatable related to selected sheet
    data_to_rotate <- eventReactive(eventExpr = {input$read_curves
      input$data_sheets},
      valueExpr = {
        dataframe_to_process(read_excel(input$read_curves$datapath, sheet = input$data_sheets))
        read_excel(input$read_curves$datapath, sheet = input$data_sheets)})
    
    
    
    output$data_to_rotate_out <- DT::renderDataTable({
      req(input$read_curves)
      req(input$data_sheets)
      data_to_rotate()
    }) 
    
    
    

# Rotating plot / box 2 - Visualization -----------------------------------

    
    
    # check_value <- reactive({
    #   
    #   input$rotate_plot[1]
    #   
    # })
    # 
    # output$area_value <- renderPrint({check_value()})
    
    
    
    
    
    # Rotating the whole plot
    
    plot_average <- eventReactive(eventExpr = {input$render_plot_with_average
                                               input$rotate_average
                                                                                                    },
                                  valueExpr = {
                                    
                                    plot <- average_curve(data_to_rotate())
                                    
                                    if (input$rotate_average == T) {
                                      
                                      st <- input$flat_start
                                      en <- input$flat_end
                                      
                                      plot <- 
                                        rotating_plot(plot, st, en)
                                        
                                    }
                                    
                                    return(plot)
                                    
                                    }, ignoreNULL = FALSE)
    
    

    # Rotating the part of the plot
    
    plot_average_part <- eventReactive(eventExpr = {
      input$render_plot_with_average
      input$rotate_average
      input$rotate_part
      input$rotate_down},
      
    valueExpr = {
      
      req(plot_average())
      
      if (input$rotate_part == T) {
        
        st <- input$line_start
        en <- input$line_end
      
        plot <- rotating_plot(plot_average(), st, en, part = input$rotate_part, shift_down = input$rotate_down)
        
        
      } else {plot <- plot_average()}
      
      return(plot)
    
    }, ignoreNULL = FALSE)
    
    
    
    
    
    
    # Rotating the whole SINGLE plot
    

    df2dim_single_and_rotated <- eventReactive(eventExpr = {input$rotate_single_plot

    },

    valueExpr = {

       df2dim <- getting_a_slice_of_df(dataframe_to_process(), input$cell_to_plot_to_rotate)

        st <- input$flat_start
        en <- input$flat_end

        df2dim <-rotating_plot(df2dim, st, en)

      return(df2dim)
      


    })
    
    
    
    
    
    
    observeEvent(input$rotate_single_plot, {
      
      output$plot_single_out <- renderPlotly({
        
        req(df2dim_single_and_rotated())
        
        
        render_plot <- df2dim_single_and_rotated()
        
        
        plot <- ggplotly_render(render_plot,
                                baseline = input$mark_line_to_rotate,
                                b_min = input$line_start,
                                b_max = input$line_end,
                                region = input$mark_line_to_rotate,
                                r_min = input$flat_start,
                                r_max = input$flat_end,
                                ready = FALSE) +
          scale_color_manual(values='black')
        
        
        
        ggplotly(plot)
        
      })
      
    })
    
    
    # Rotating the part of the SINGLE plot
    
    df2dim_single_and_rotated_part <- eventReactive(eventExpr = {input$rotate_single_plot_part
                                                                 input$rotate_single_down  
      
    },
    
    valueExpr = {
      
      df2dim <- getting_a_slice_of_df(dataframe_to_process(), input$cell_to_plot_to_rotate)
      
      st <- input$line_start
      en <- input$line_end
      
      df2dim <-rotating_plot(df2dim, st, en, part = TRUE, shift_down = input$rotate_single_down)
      
      return(df2dim)
      
      
      
    })
    
    
    
    observeEvent(input$rotate_single_plot_part, {
      
      output$plot_single_out <- renderPlotly({
        
        req(df2dim_single_and_rotated_part())
        
        
        render_plot <- df2dim_single_and_rotated_part()
        
        
        plot <- ggplotly_render(render_plot,
                                baseline = input$mark_line_to_rotate,
                                b_min = input$line_start,
                                b_max = input$line_end,
                                region = input$mark_line_to_rotate,
                                r_min = input$flat_start,
                                r_max = input$flat_end,
                                ready = FALSE) +
          scale_color_manual(values='black')
        
        
        
        ggplotly(plot)
        
      })
      
    })
    
    
    
    
    
    observeEvent(input$save_single_changes, {

      dataframe_to_process_new <- replace_columns_in_dfs(dataframe_to_process(), df2dim_single_and_rotated())
      dataframe_to_process(dataframe_to_process_new)
      
      dataframe_to_process_new <- replace_columns_in_dfs(dataframe_to_process(), df2dim_single_and_rotated_part())
      dataframe_to_process(dataframe_to_process_new)
      
      
      
      output$plot_single_out2 <- renderPlotly({

        pl <- getting_a_slice_of_df(dataframe_to_process(), input$cell_to_plot_to_rotate)
        
        plot <- ggplotly_render(pl,
                                baseline = input$mark_line_to_rotate,
                                b_min = input$line_start,
                                b_max = input$line_end,
                                region = input$mark_line_to_rotate,
                                r_min = input$flat_start,
                                r_max = input$flat_end,
                                ready = FALSE) +
          scale_color_manual(values='black')
        
        
        
        ggplotly(plot)  

        })
    })
    
    
    
    

    # # Rotating the part of the plot
    # 
    # plot_average_part <- eventReactive(eventExpr = {
    #   input$render_plot_with_average
    #   input$rotate_average
    #   input$rotate_part
    #   input$rotate_down},
    #   
    #   valueExpr = {
    #     
    #     req(plot_average())
    #     
    #     if (input$rotate_part == T) {
    #       
    #       st <- input$line_start
    #       en <- input$line_end
    #       
    #       plot <- rotating_curve(plot_average(), st, en, input$rotate_down)
    #       
    #       
    #     } else {plot <- plot_average()}
    #     
    #     return(plot)
    #     
    #   }, ignoreNULL = FALSE)
    
    
    
    
    
    
    
    # Mark lines on the plot
    
    plot_average_output <- eventReactive(eventExpr = {input$render_plot_with_average
                                                      input$rotate_average
                                                      input$mark_line_to_rotate
                                                      input$rotate_part
                                                      input$rotate_down
                                                      input$line_start
                                                      input$line_end},
    valueExpr = {
      
      req(plot_average_part())
      
      baseline <- input$mark_line_to_rotate
      region <- input$mark_line_to_rotate
      
      
      curve <- ggplotly_render(plot_average_part(), 
                                 baseline, 
                                 b_min = input$line_start, 
                                 b_max = input$line_end, 
                                 region,
                                 r_min = input$flat_start,
                                 r_max = input$flat_end,
                                 ready = FALSE) +
        scale_color_manual(values='black')
      
      return(ggplotly(curve))
      
      
    }, ignoreNULL = FALSE)
    

    
    # Render average plot
    
    observeEvent(input$render_plot_with_average, {

      output$plot_average_out <- renderPlotly({plot_average_output()}) # output$plot_average_out



    }) # /level 1, observeEvent input$render_plot_with_average
    
    
    
    

# Single PLOT -------------------------------------------------------------

    
    dataframe_to_process <- reactiveVal(NULL)
    


    
    # Plotting single graph and rotate
    
    observeEvent(input$plot_single_to_rotate, {
      

      output$plot_single_out <- renderPlotly({
        
        req(data_to_rotate())
        req(input$cell_to_plot_to_rotate)
        

        render_plot <- getting_a_slice_of_df(data_to_rotate(), input$cell_to_plot_to_rotate)
        
        
        plot <- ggplotly_render(render_plot,
                                baseline = input$mark_line_to_rotate,
                                b_min = input$line_start,
                                b_max = input$line_end,
                                region = input$mark_line_to_rotate,
                                r_min = input$flat_start,
                                r_max = input$flat_end,
                                ready = FALSE) +
          scale_color_manual(values='black')
        
        
        
        ggplotly(plot)
        
      })
      
      output$data_to_rotate_out2 <- DT::renderDataTable({
        req(data_to_rotate())
        dataframe_to_process()
      }) 
      
    }) # /level 1, observeEvent input$plot_single_to_rotate
    
    
    
    # Reset switchInput values
    
    observeEvent(input$reset_plot, {
      
      updateSwitchInput(
        inputId = "mark_line_to_rotate",
        value = FALSE
      )
      
      updateSwitchInput(
        inputId = "rotate_average",
        value = FALSE
      )
      
      updateSwitchInput(
        inputId = "rotate_part",
        value = FALSE
      )
      
      updateSwitchInput(
        inputId = "rotate_down",
        value = FALSE
      )
      
                }) # output$reset_plot
    
    
    
    # Saving data with Average column
    
    output$SaveFinal <- downloadHandler(
      filename = function() {filename(input$read_curves$name, "Rotated.xlsx")},
      content = function(file) {
        df_list <- list('first' = plot_average_part(),
                        'second' = average_curve(data_to_rotate()))
        names(df_list) <- c(paste0(input$sheets, '_rotated'), paste0(input$sheets, '_initial'))
        write_xlsx(df_list, path = file)
      }
    )
    
    
    
    

# Rotating plot / box 2 - Plot rotated result -----------------------------

    
    
    
    
    # Render plot
    
    
    plot_average_output2 <- eventReactive(eventExpr = {input$render_plot_with_average
      input$rotate_average
      input$rotate_part
      input$rotate_down
      input$baseline_start 
      input$baseline_end 
      input$area_start
      input$area_end      
      
      },

      valueExpr = {
        
        req(plot_average_part())
        
        baseline <- input$mark_line_to_rotate
        
        curve <- ggplotly_render(plot_average_part(), 
                                 baseline = T, 
                                 b_min = input$baseline_start, 
                                 b_max = input$baseline_end, 
                                 region = T,
                                 r_min = input$area_start,
                                 r_max = input$area_end,
                                 ready = FALSE) +
          scale_color_manual(values='black')
        
        return(ggplotly(curve))
        
        
      }, ignoreNULL = FALSE)
    
    
    
    observeEvent(input$plot_rotated_result, {
      
      output$plot_average_out2 <- renderPlotly({plot_average_output2()}) # output$plot_average_out
      
      
      
    })
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
        
    
} # level 0