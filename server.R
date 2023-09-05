source('engine.R')







# Define server logic to read selected file ----

server <- function(input, output) {

  

# Preliminary analysis -----------------------------------------------------    
  
# Reading sheet names in Excel file
  
  sheets_in_the_dataTS <-  eventReactive(eventExpr = {input$dataTS},
                                       valueExpr ={
                                         excel_sheets(input$dataTS$datapath)
                                       })
  
# Following the user's choice for sheets to process
  
  observe({
    
    if (input$cRatio == F) {
          shinyjs::disable("sheetRatio")
        } else if (input$cRatio == T) {
          shinyjs::enable("sheetRatio")
        }
    if (input$cNum == F) {
          shinyjs::disable("sheetNum")
        } else if (input$cNum == T) {
          shinyjs::enable("sheetNum")
        }
    if (input$cDen == F) {
          shinyjs::disable("sheetDen")
        } else if (input$cDen == T) {
          shinyjs::enable("sheetDen")
        }
  })
  
  
# Suggesting sheets and updating input information 

  observeEvent(input$dataTS,{
    
    updateSelectInput(inputId = 'sheetRatio',
                      choices = sheets_in_the_dataTS(),
                      selected = str_extract(sheets_in_the_dataTS(), '^[Rr]atio$')
                      )
    
    updateSelectInput(inputId = 'sheetNum',
                      choices = sheets_in_the_dataTS(),
                      selected = str_extract(sheets_in_the_dataTS(), '^340$')
                      )
    
    updateSelectInput(inputId = 'sheetDen',
                      choices = sheets_in_the_dataTS(),
                      selected = str_extract(sheets_in_the_dataTS(), '^380$')
                      )
    
  })
  
  
  

# Change the background color of the button when pressed ------------------


  observeEvent(input$correct_time,{
    
    if((input$correct_time%%2) == 1) {
    runjs('document.getElementById("correct_time").style.backgroundColor = "green";')
    } else {
    runjs('document.getElementById("correct_time").style.backgroundColor = "grey";')
    }
  })
  
  observeEvent(input$change_names_button,{

      runjs('document.getElementById("change_names_button").style.backgroundColor = "green";')

  })
  
  
  
# Preliminary analysis/ 1st box -------------------------------------------

# All 4 tables rendering
  
  #  RATIO
  df_ratio_ready <- eventReactive(eventExpr = {input$dataTS 
    input$disp
    input$correct_time
    input$change_names
    input$change_names_button
    input$cRatio
    input$sheetRatio},
    valueExpr = {
      req(input$dataTS)
      req(input$cRatio)
      reading_xls(input$dataTS, 
                  disp_opt=input$disp, 
                  input$correct_time,
                  input$step,
                  input$change_names, 
                  input$cellName, 
                  sheet_n = input$sheetRatio)
    })
  
  
  
  output$df_ratio <- DT::renderDataTable({
    req(input$dataTS)
    req(input$cRatio)
    customDT(df_ratio_ready())
  })
  
  
  # NUMERATOR
  df_Num_ready <- eventReactive(eventExpr = {input$dataTS 
                                             input$disp
                                             input$correct_time
                                             input$change_names
                                             input$change_names_button
                                             input$cNum
                                             input$sheetNum},
                                valueExpr = {
                                  req(input$dataTS)
                                  req(input$cNum)
                                  reading_xls(input$dataTS, 
                                              disp_opt=input$disp, 
                                              input$correct_time,
                                              input$step, 
                                              input$change_names, 
                                              input$cellName, 
                                              sheet_n = input$sheetNum)
                                }) 
  
  output$df_Num <- DT::renderDataTable({
    req(input$dataTS)
    req(input$cNum)
    customDT(df_Num_ready())
                                  }) 
    
    
  # DENOMINATOR
  df_Den_ready <- eventReactive(eventExpr = {input$dataTS 
                                             input$disp
                                             input$correct_time
                                             input$change_names
                                             input$change_names_button
                                             input$cDen
                                             input$sheetDen},
                                valueExpr = {
                                  req(input$dataTS)
                                  req(input$cDen)
                                  reading_xls(input$dataTS, 
                                              disp_opt=input$disp, 
                                              input$correct_time,
                                              input$step, 
                                              input$change_names, 
                                              input$cellName, 
                                              sheet_n = input$sheetDen)
                                            })
  
  output$df_Den <- DT::renderDataTable({    
    req(input$dataTS)
    req(input$cDen)
    customDT(df_Den_ready())
 })
  
  
  


  # NUMERATOR/DENOMINATOR
  df_custom_ratio_ready <- eventReactive(eventExpr = {input$dataTS 
                                                      input$disp
                                                      input$correct_time
                                                      input$change_names
                                                      input$change_names_button
                                                      input$cNum
                                                      input$cDen},
                                          valueExpr = {
                                            req(input$dataTS)
                                            req(input$cNum)
                                            req(input$cDen)
                                            custom_ratio(df_Num_ready(), df_Den_ready())}) 

  output$df_custom_ratio <- DT::renderDataTable({  
    req(input$dataTS)
    req(df_Num_ready)
    req(df_Den_ready)
    customDT(df_custom_ratio_ready())
    }) 
  
  
  
    

    
# Save as excel file
  
  output$SaveXlsBox1 <- downloadHandler(
    filename = function() { filename(input$dataTS, "ProcessedTable.xlsx")},
    content = function(file) {
      
      
      wb <- createWorkbook()
      
      if (input$cRatio) {
        sheet1 <- addWorksheet(wb, sheetName = "Ratio")
        writeDataTable(wb, sheet1, df_ratio_ready())
      }
      
      if (input$cNum) {
        sheet2 <- addWorksheet(wb, sheetName = "Numerator")
        writeDataTable(wb, sheet2, df_Num_ready())
      }
      
      if (input$cDen) {
        sheet3 <- addWorksheet(wb, sheetName = "Denominator")
        writeDataTable(wb, sheet3, df_Den_ready())
      }
      
      if (input$cNum & input$cDen) {
        sheet4 <- addWorksheet(wb, sheetName = "Custom_ratio")
        writeDataTable(wb, sheet4, df_custom_ratio_ready())
      }
      
      saveWorkbook(wb, file)
      
    })
  

  
  
# Preliminary analysis/ 2d box - STATISTICS -------------------------------------------  
  
# RATIO statistics
  
  df_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                       
                                       valueExpr = {
                                         req(input$dataTS)
                                         req(input$cRatio)
                                         reading_xls(input$dataTS, 
                                                     disp_opt="all", 
                                                     input$correct_time,
                                                     input$step, 
                                                     input$change_names, 
                                                     input$cellName, 
                                                     sheet_n = input$sheetRatio)
                                       })

  df_ratio_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                             
                                             valueExpr = {
                                               req(input$dataTS)
                                               req(input$cRatio)
                                               return(basic_statistics(df_ratio_basic_stat()))
                                               
                                             }) 
  
  output$df_ratio_basic_stat_out <- DT::renderDataTable({
    css_styles_DT(df_ratio_basic_stat_table())
    
  }) 
  
  
  
# NUMERATOR statistics
  
  df_Num_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$dataTS)
                                       req(input$cNum)
                                       reading_xls(input$dataTS, 
                                                   disp_opt="all", 
                                                   input$correct_time,
                                                   input$step, 
                                                   input$change_names, 
                                                   input$cellName, 
                                                   sheet_n = input$sheetNum) 
                                       
                                       }) 
  
  df_Num_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                           
                                           valueExpr = {
                                             req(input$dataTS)
                                             req(input$cNum)
                                             return(basic_statistics(df_Num_basic_stat()))
                                             
                                           })
  
  output$df_Num_basic_stat_out <- DT::renderDataTable({
    css_styles_DT(df_Num_basic_stat_table())

  }) 
  
  
# DENOMINATOR statistics
  
  df_Den_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                     
                                     valueExpr = {
                                       req(input$dataTS)
                                       req(input$cDen)
                                       reading_xls(input$dataTS, 
                                                   disp_opt="all", 
                                                   input$correct_time,
                                                   input$step, 
                                                   input$change_names, 
                                                   input$cellName, 
                                                   sheet_n = input$sheetDen) 
                                       
                                     })
  
  
  df_Den_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                           
                                           valueExpr = {
                                             req(input$dataTS)
                                             req(input$cDen)
                                             return(basic_statistics(df_Den_basic_stat()))
                                             
                                           })
  
  output$df_Den_basic_stat_out <- DT::renderDataTable({
    css_styles_DT(df_Den_basic_stat_table())

    
  }) 
  
  
# CUSTOM RATIO statistics 

  df_custom_ratio_basic_stat <- eventReactive(eventExpr = {input$basicStat}, 
                                      valueExpr = {
                                        req(input$dataTS)
                                        req(input$cNum)
                                        req(input$cDen)
                                        custom_ratio(df_Num_basic_stat(), df_Den_basic_stat())
                                      }) 
  
  df_custom_ratio_basic_stat_table <- eventReactive(eventExpr = {input$basicStat},
                                             
                                             valueExpr = {
                                               req(input$dataTS)
                                               req(input$cNum)
                                               req(input$cDen)
                                               return(basic_statistics(df_custom_ratio_basic_stat()))
                                               
                                             }) 
  
  output$df_custom_ratio_basic_stat_out <- DT::renderDataTable({
    css_styles_DT(df_custom_ratio_basic_stat_table())

  }) 
  
# Save BASIC STATISTICS as excel file
  
  output$SaveXlsBoxStat <- downloadHandler(
    filename = function() {filename(input$dataTS, "BasicStatisticsTable.xlsx")},
    content = function(file) {
      
      wb <- createWorkbook()
      
      if (input$cRatio) {
        sheet1 <- addWorksheet(wb, sheetName = "Ratio")
        writeDataTable(wb, sheet1, df_ratio_basic_stat_table())
      }
      
      if (input$cNum) {
        sheet2 <- addWorksheet(wb, sheetName = "Numerator")
        writeDataTable(wb, sheet2, df_Num_basic_stat_table())
      }
      
      if (input$cDen) {
        sheet3 <- addWorksheet(wb, sheetName = "Denominator")
        writeDataTable(wb, sheet3, df_Den_basic_stat_table())
      }
      
      if (input$cNum & input$cDen) {
        sheet4 <- addWorksheet(wb, sheetName = "Custom_ratio")
        writeDataTable(wb, sheet4, df_custom_ratio_basic_stat_table())
      }
      
      saveWorkbook(wb, file)
      
      

      })

# Preliminary analysis/ 3d box, plots------------------------------------------------

  # # Debugging------------------------------------------------
  # output$inputValues <- renderPrint({
  #   inputList <- reactiveValuesToList(input)
  #   inputList
  # })
  
  
  
  
  
# Rendering SelectInput for columns choosing process
  
    observeEvent(input$basicStat, {
      
      runjs('document.getElementById("basicStat").style.backgroundColor = "green";')

      output$tabUI <- renderUI({
        if (input$tab == "R") {
          selectInput("ratioInput", "Ratio cells names", 
                      choices = colnames(df_ratio_basic_stat())[-1], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$ratioInput))
          
        } else if (input$tab == "N") {
          selectInput("numInput", "Numerator cells names", 
                      choices = colnames(df_Num_basic_stat())[-1], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$numInput))
          
        } else if (input$tab == "D") {
          selectInput("denInput", "Denominator cells names", 
                      choices = colnames(df_Den_basic_stat())[-1], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$denInput))
          
        } else if (input$tab == "ND") {
          selectInput("customRatioInput", "Num/Den cells names", 
                      choices = colnames(df_custom_ratio_basic_stat())[-1], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$customRatioInput)
                      )
        }
        
        
      })
      
      
      req(df_ratio_basic_stat())
      
      shinyalert(type = 'success', 
                 text = "The statistics have been calculated!",
                 closeOnClickOutside = T,
                 timer = 2000,
                 showConfirmButton = F)
      
    })
  
  
  
  
# Rendering ALL plots

    observeEvent(input$plot_all, {
      

      
      output$plot_ratio <- renderPlotly({
        req(input$plot_all, df_ratio_basic_stat())
        ggplotly_render(df_ratio_basic_stat(), 
                        rcolor = color_palette(df_ratio_basic_stat(), rmcellValues$colors2000),
                        sorting = input$legend_order)})
      
      
      output$plotNum <- renderPlotly({
        req(input$plot_all, df_Num_basic_stat())
        ggplotly_render(df_Num_basic_stat(), 
                        rcolor = color_palette(df_Num_basic_stat(), rmcellValues$colors2000),
                        sorting = input$legend_order)})
      
      
      output$plotDen <- renderPlotly({
        req(input$plot_all, df_Den_basic_stat())
        ggplotly_render(df_Den_basic_stat(), 
                        rcolor = color_palette(df_Den_basic_stat(), rmcellValues$colors2000),
                        sorting = input$legend_order)})
      
      
      output$plot_custom_ratio <- renderPlotly({
        req(input$plot_all, df_custom_ratio_basic_stat())
        ggplotly_render(df_custom_ratio_basic_stat(), 
                        rcolor = color_palette(df_custom_ratio_basic_stat(), rmcellValues$colors2000),
                        sorting = input$legend_order)})
    
      
    }) 
    
    
    
    
    
# Rendering SINGLE plot
    
    observeEvent(input$plot_single, {
      
      
      
      output$plot_ratio <- renderPlotly({
        req(df_ratio_basic_stat())
        
        display_single_plot(df_ratio_basic_stat(), input$ratioInput)

      })     
      

      output$plotNum <- renderPlotly({
        req(df_Num_basic_stat())
        
        display_single_plot(df_Num_basic_stat(), input$numInput)
        
      }) 
      
      
      output$plotDen <- renderPlotly({
        req(df_Den_basic_stat())
        
        display_single_plot(df_Den_basic_stat(), input$denInput)
        
      }) 
      
      
      

      output$plot_custom_ratio <- renderPlotly({
        req(df_custom_ratio_basic_stat())
        
        display_single_plot(df_custom_ratio_basic_stat(), input$customRatioInput)
        
      })
  
  
    }) 


    
# Excluding cells ---------------------------------------------------------
    
    
# Button to obtain new dataframes without bad cells information    
    

    # Now creating reactive values list of cells to exclude
    rmcellValues <- reactiveValues()
    
    #  This value represents a color palette, that is used for multiple plots rendering
    rmcellValues$colors2000 <- randomColor(count = 2000, hue = 'random', luminosity = 'bright')
      
    
# Buttons to Exclude/Undo/Include/Reset cells 
    
    observeEvent(input$exclude_cell, {
      
      # List of column names to exclude from dataframe
      if (input$tab == "R") {
        
        rmcellValues$cList <- unique(c(shiny::isolate(rmcellValues$cList), 
                                       shiny::isolate(input$ratioInput)
                                       )
                                     )
        
      } else if (input$tab == "N") {
        
        rmcellValues$cList <- unique(c(shiny::isolate(rmcellValues$cList), 
                                       shiny::isolate(input$numInput)
                                       )
                                     )

        
      } else if (input$tab == "D") {
        
        rmcellValues$cList <- unique(c(shiny::isolate(rmcellValues$cList), 
                                       shiny::isolate(input$denInput)
                                       )
                                     )

        
      } else if (input$tab == "ND") {
        
        rmcellValues$cList <- unique(c(shiny::isolate(rmcellValues$cList), 
                                       shiny::isolate(input$customRatioInput)
                                       )
                                     )
        
      }


      # Printing the list of excluded column names
      output$list_of_cells<-renderPrint({
        gtools::mixedsort(rmcellValues$cList, decreasing = T)
                                         })
      })
    
    
    # Reset (empty) the list of excluded column names
    observeEvent(input$exclude_reset, {
      rmcellValues$cList <- c()
      
      
      output$list_of_cells<-renderPrint({
        print("'Reset' button have been pressed! Nothing to exclude!")
                                        })
      
    })
    
    
    # Undo the last action
    observeEvent(input$exclude_undo, {
      req(rmcellValues$cList)
      rmcellValues$cList <- rmcellValues$cList[-length(rmcellValues$cList)]
      
      
      output$list_of_cells<-renderPrint({
        
        if (identical(rmcellValues$cList, character(0))) {
          
          print('Nothing to exclude!')
          
        } else {
        
        gtools::mixedsort(rmcellValues$cList, decreasing = T)
          
               }
      })
      
    }) 
    
    
    # Return a specific column name back to the initial dataframe (leave this column name, do not exclude it)
    observeEvent(input$include_cell, {
      req(rmcellValues$cList)
      
      if (input$tab == "R") {
        
        rmcellValues$cList <- 
          rmcellValues$cList[!rmcellValues$cList == shiny::isolate(input$ratioInput)]

        
      } else if (input$tab == "N") {
        
        rmcellValues$cList <- 
          rmcellValues$cList[!rmcellValues$cList == shiny::isolate(input$numInput)]

        
      } else if (input$tab == "D") {
        
        rmcellValues$cList <- 
          rmcellValues$cList[!rmcellValues$cList == shiny::isolate(input$denInput)]

        
      } else if (input$tab == "ND") {
        
        rmcellValues$cList <- 
          rmcellValues$cList[!rmcellValues$cList == shiny::isolate(input$customRatioInput)]

        
      }
      
      output$list_of_cells<-renderPrint({
        
        if (identical(rmcellValues$cList, character(0))) {
          
          print('Nothing to exclude!')
          
        } else {
          
          gtools::mixedsort(rmcellValues$cList, decreasing = T)
          
        }
      })
    }) 
    
    
    

# New dataframes without excluded column names
    
    observeEvent(input$new_dataframes,{
      
      req(df_ratio_excluded())
      
      shinyalert(type = 'success', 
                 text = "New tables without excluded traces are obtained!",
                 closeOnClickOutside = T,
                 timer = 2000,
                 showConfirmButton = F)
      
      if((input$correct_time%%2) == 1) {
        runjs('document.getElementById("new_dataframes").style.backgroundColor = "green";')
      } else {
        runjs('document.getElementById("new_dataframes").style.backgroundColor = "grey";')
      }
    })
    
    
    
      df_ratio_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                       
                           valueExpr = {
                             
                             req(input$dataTS)
                             req(input$cRatio)                  
                             subset(df_ratio_basic_stat(), 
                                    select = !(colnames(df_ratio_basic_stat()) %in% rmcellValues$cList))
                             
                           }) 
    
      
      df_Num_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 

                          valueExpr = {
                            
                            req(input$dataTS)
                            req(input$cNum)                  
                            subset(df_Num_basic_stat(), 
                                 select = !(colnames(df_Num_basic_stat()) %in% rmcellValues$cList))
                          
                          }) 
    
    
      df_Den_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                       
                         valueExpr = {
                           
                           req(input$dataTS)
                           req(input$cDen)                  
                           subset(df_Den_basic_stat(), 
                                  select = !(colnames(df_Den_basic_stat()) %in% rmcellValues$cList))
                         
                         }) 
      

      
      df_custom_ratio_excluded <- eventReactive(eventExpr = {input$new_dataframes}, 
                                         
                                  valueExpr = {
                                     
                                     req(input$dataTS)
                                     req(input$cNum)   
                                     req(input$cDen)
                                     subset(df_custom_ratio_basic_stat(), 
                                            select = !(colnames(df_custom_ratio_basic_stat()) %in% rmcellValues$cList))
                                     
                                  })

    
# Rendering new plots without excluded column names 
      
      observeEvent(input$plot_new_all, {
        
        output$plotNum <- renderPlotly({
          req(input$plot_new_all, df_Num_excluded())
          ggplotly_render(df_Num_excluded(), rcolor = rmcellValues$colors2000)})
      
        output$plotDen <- renderPlotly({
          req(input$plot_new_all, df_Den_excluded())
          ggplotly_render(df_Den_excluded(), rcolor = rmcellValues$colors2000)})
        
        output$plot_ratio <- renderPlotly({
          req(input$plot_new_all, df_ratio_excluded())
          ggplotly_render(df_ratio_excluded(), rcolor = rmcellValues$colors2000)})
        
        output$plot_custom_ratio <- renderPlotly({
          req(input$plot_new_all, df_custom_ratio_excluded())
          ggplotly_render(df_custom_ratio_excluded(), rcolor = rmcellValues$colors2000)})
        
        })

    
    
# Save DATA WITHOUT BAD CELLS as excel file
    
      output$SaveXlsBoxNoBadCells <- downloadHandler(
        filename = function() {filename(input$dataTS, "CleanTable.xlsx")},
        content = function(file) {
          
          
          wb <- createWorkbook()
          
          if (input$cRatio) {
            sheet1 <- addWorksheet(wb, sheetName = "Ratio")
            writeDataTable(wb, sheet1, df_ratio_excluded())
          }
          
          if (input$cNum) {
            sheet2 <- addWorksheet(wb, sheetName = "Numerator")
            writeDataTable(wb, sheet2, df_Num_excluded())
          }
          
          if (input$cDen) {
            sheet3 <- addWorksheet(wb, sheetName = "Denominator")
            writeDataTable(wb, sheet3, df_Den_excluded())
          }
          
          if (input$cNum & input$cDen) {
            sheet4 <- addWorksheet(wb, sheetName = "Custom_ratio")
            writeDataTable(wb, sheet4, df_custom_ratio_excluded())
          }
          
          saveWorkbook(wb, file)
          
      })
    


    
    

# Analyzing amplitude -----------------------------------------------------    
    
    
# Reading sheet names in Excel file
      
      sheets_in_a_clean_file <-  eventReactive(eventExpr = {input$clean_file},
                                             valueExpr ={
                                               excel_sheets(input$clean_file$datapath)
                                             })
      
# Following the user's choice for sheets to process
      
      observe({
        
        if (input$clRatio == F) {
          shinyjs::disable("sheetClRatio")
        } else if (input$clRatio == T) {
          shinyjs::enable("sheetClRatio")
        }
        if (input$clNum == F) {
          shinyjs::disable("sheetClNum")
        } else if (input$clNum == T) {
          shinyjs::enable("sheetClNum")
        }
        if (input$clDen == F) {
          shinyjs::disable("sheetClDen")
        } else if (input$clDen == T) {
          shinyjs::enable("sheetClDen")
        }
      })
      
      
# Suggesting sheets and updating input information 
      
      observeEvent(input$clean_file, {
        
        updateSelectInput(inputId = 'sheetClRatio',
                          choices = sheets_in_a_clean_file(),
                          selected = str_extract(sheets_in_a_clean_file(), '^[Rr]atio$')
        )
        
        updateSelectInput(inputId = 'sheetClNum',
                          choices = sheets_in_a_clean_file(),
                          selected = str_extract(sheets_in_a_clean_file(), '^340$')
        )
        
        updateSelectInput(inputId = 'sheetClDen',
                          choices = sheets_in_a_clean_file(),
                          selected = str_extract(sheets_in_a_clean_file(), '^380$')
        )
        
      })
      
      
# All 4 tables of CLEAN DATA rendering
      
      
      
    # Ratio 
    df_ratio_clean <- eventReactive(eventExpr = {input$sheetClRatio
      input$clean_file},
      valueExpr = {
        req(input$clean_file)
        req(input$clRatio)
        read_excel(input$clean_file$datapath,
                   sheet=input$sheetClRatio)
        
      }
    ) 
    
    
    output$cl_ratio <- DT::renderDataTable({
      req(input$clean_file)
      req(input$clRatio)
      df_ratio_clean()
    })     

    
    # Num
    df_Num_clean <- eventReactive(eventExpr = {input$sheetClNum},
                    valueExpr = {
                      req(input$clean_file)
                      read_excel(input$clean_file$datapath,
                                 sheet=input$sheetClNum)
                      })
    
    output$cl_Num <- DT::renderDataTable({
      req(input$clean_file)
      req(input$clNum)
      df_Num_clean()
                                            }) 
    

    # Den
    df_Den_clean <- eventReactive(eventExpr = {input$sheetClDen},
      valueExpr = {
        req(input$clean_file)
        read_excel(input$clean_file$datapath,
                   sheet=input$sheetClDen)
      }
    ) 
    
    
    output$cl_Den <- DT::renderDataTable({
      req(input$clean_file)
      req(input$clDen)
      df_Den_clean()
    }) 

    
    # Custom Ratio 
    df_custom_ratio_clean <- eventReactive(eventExpr = {input$sheetClNum
                                                        input$sheetClDen},
      valueExpr = {
        req(input$clean_file)
        req(input$clDen)
        req(input$clNum)
        custom_ratio(df_Num_clean(), df_Den_clean())
        
      }
    ) 
    
    
    output$cl_custom_ratio <- DT::renderDataTable({
      req(input$clean_file)
      req(input$clNum)
      req(input$clDen)
      df_custom_ratio_clean()
    })    
   
    
    
    observeEvent(input$clean_file, {
      
      output$tabUI2 <- renderUI({
        
        if (input$tab2 == "R") {
          selectInput("ratioInput2", "Ratio cells names", 
                      choices = colnames(df_ratio_clean())[!grepl("^[Tt]ime", colnames(df_ratio_clean()))], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$ratioInput2))
          
        } else if (input$tab2 == "N") {
          selectInput("numInput2", "Numerator cells names", 
                      choices = colnames(df_Num_clean())[!grepl("^[Tt]ime", colnames(df_Num_clean()))], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$numInput2))
          
        } else if (input$tab2 == "D") {
          selectInput("denInput2", "Denominator cells names", 
                      choices = colnames(df_Den_clean())[!grepl("^[Tt]ime", colnames(df_Den_clean()))], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$denInput2))
          
        } else if (input$tab2 == "ND") {
          selectInput("customRatioInput2", "Num/Den cells names", 
                      choices = colnames(df_custom_ratio_clean())[!grepl("^[Tt]ime", colnames(df_custom_ratio_clean()))], 
                      selectize = FALSE, 
                      selected = shiny::isolate(input$customRatioInput2)
          )
        }
        
        
      })
      
      time_column_name <- reactive({
        df_ratio_clean()[grepl("^[Tt]ime", colnames(df_ratio_clean()))]
        })
      
      
      
      output$slider <- renderUI({
        
        min_time <- min(time_column_name())
        
        max_time <- max(time_column_name())
        
        quater <- round((max_time - min_time)/4, digits =0)
        
        
        if (input$peaks_amount == 1) {
          
          sliderInput("slider_output1", "Range 1:",
                      min = min_time, max = max_time,
                      value = c(0,quater))
          
        } else if (input$peaks_amount == 2) {
          
          tagList(
            
            sliderInput("slider_output1", "Range 1:",
                        min = min_time, max = max_time,
                        value = c(0,quater)),
            sliderInput("slider_output2", "Range 2:",
                        min = min(time_column_name()), max = max(time_column_name()),
                        value = c(quater,2*quater))
          )
          
        } else if (input$peaks_amount == 3) {
          
          tagList(
            
          sliderInput("slider_output1", "Range 1:",
                      min = min_time, max = max_time,
                      value = c(0,quater)),
          
          sliderInput("slider_output2", "Range 2:",
                      min = min(time_column_name()), max = max(time_column_name()),
                      value = c(quater,2*quater)),
          
          sliderInput("slider_output3", "Range 3:",
                      min = min(time_column_name()), max = max(time_column_name()),
                      value = c(2*quater,3*quater))
          )
          
        } else if (input$peaks_amount == 4) {
          
          tagList(
          
          sliderInput("slider_output1", "Range 1:",
                      min = min_time, max = max_time,
                      value = c(0,quater)),
          
          sliderInput("slider_output2", "Range 2:",
                      min = min(time_column_name()), max = max(time_column_name()),
                      value = c(quater,2*quater)),
          
          sliderInput("slider_output3", "Range 3:",
                      min = min(time_column_name()), max = max(time_column_name()),
                      value = c(2*quater,3*quater)),
          
          sliderInput("slider_output4", "Range 4:",
                      min = min(time_column_name()), max = max(time_column_name()),
                      value = c(3*quater,max_time))
          )
          
        }
        

        
      })
      
      
      
      output$verbatim <- renderText({
        
        typeof(input$slider_output4)
        
        })
      

      
    })
    
# Rendering ALL plots

    observeEvent(input$plot_all2, {
      
      
      
      output$plot_ratio2 <- renderPlotly({
        req(input$plot_all2, df_ratio_clean())
        plot_object <- ggplotly_render(df_ratio_clean(), 
                        rcolor = color_palette(df_ratio_clean(), rmcellValues$colors2000),
                        sorting = input$legend_order2,
                        baseline = T, 
                        b_min = 0, 
                        b_max = 120, 
                        ready = FALSE
                        )
        
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
        })
      
      
      output$plotNum2 <- renderPlotly({
        req(input$plot_all2, df_Num_clean())
        plot_object <- ggplotly_render(df_Num_clean(), 
                        rcolor = color_palette(df_Num_clean(), rmcellValues$colors2000),
                        sorting = input$legend_order2,
                        baseline = T, 
                        b_min = 0, 
                        b_max = 120, 
                        ready = FALSE
        )
        
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      })
      
      
      output$plotDen2 <- renderPlotly({
        req(input$plot_all2, df_Den_clean())
        plot_object <- ggplotly_render(df_Den_clean(), 
                        rcolor = color_palette(df_Den_clean(), rmcellValues$colors2000),
                        sorting = input$legend_order2,
                        baseline = T, 
                        b_min = 0, 
                        b_max = 120, 
                        ready = FALSE
        )
        
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      })
      
      
      output$plot_custom_ratio2 <- renderPlotly({
        req(input$plot_all2, df_custom_ratio_clean())
        plot_object <- ggplotly_render(df_custom_ratio_clean(), 
                        rcolor = color_palette(df_custom_ratio_clean(), rmcellValues$colors2000),
                        sorting = input$legend_order2,
                        baseline = T, 
                        b_min = 0, 
                        b_max = 120, 
                        ready = FALSE
        )
        
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      })
      
      
    }) 
    
    
    
    
    
# Rendering SINGLE plot
    
    observeEvent(input$plot_single2, {
      
      
      output$plot_ratio2 <- renderPlotly({
        req(df_ratio_clean())
        
        plot_object <- display_single_plot(df_ratio_clean(), input$ratioInput2, ready = F, lines = T)
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      })     
      
      
      output$plotNum2 <- renderPlotly({
        req(df_Num_clean())
        
        plot_object <- display_single_plot(df_Num_clean(), input$numInput2)
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      }) 
      
      
      output$plotDen2 <- renderPlotly({
        req(df_Den_clean())
        
        plot_object <- display_single_plot(df_Den_clean(), input$denInput2)
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      }) 
      
      
      output$plot_custom_ratio2 <- renderPlotly({
        req(df_custom_ratio_clean())
        
        plot_object <- display_single_plot(df_custom_ratio_clean(), input$customRatioInput2)
        pl <- plot_wrapper(plot_object, input$slider_output1, input$slider_output2, input$slider_output3, input$slider_output4)
        
        return(ggplotly(pl))
        
      })
      
      
    }) 
    

# Calculating amplitudes --------------------------------------------------

    
    # Num
    df_Num_amplitude <- eventReactive(eventExpr = {input$amplitudeStat
      input$clNum},
      valueExpr = {
        req(input$clean_file)
        req(input$clNum)
        find_amplitude(df_Num_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_Num_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$clNum)
      df_Num_amplitude()
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
      req(input$clNum)
      req(input$clDen)
      df_custom_ratio_amplitude()
    })
    
    
    
# IN THIS CASE WE NEED TO FIND MINIMUM INSTEAD! 
    
    # Den
    df_Den_amplitude <- eventReactive(eventExpr = {input$amplitudeStat
      input$clDen},
      valueExpr = {
        req(input$clean_file)
        req(input$clDen)
        find_amplitude_Den(df_Den_clean(), input$min_time, input$max_time, input$start_time, input$end_time)
        
      }
    )
    
    output$df_Den_amplitude_out <- DT::renderDataTable({
      req(input$clean_file)
      req(input$amplitudeStat)
      req(input$clDen)
      df_Den_amplitude()
    })    
    
    
# Summary for amplitudes
    
    # Num
    df_Num_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_Num_amplitude(), df_excluded_cells_list())
                                    })
    
    # Den
    df_Den_summary <- eventReactive(eventExpr = {input$amplitudeStat},
                                    valueExpr = {
                                      req(input$clean_file)
                                      summarize_amplitudes(df_Den_amplitude(), df_excluded_cells_list())
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
      filename = function() {filename(input$dataTS, "Amplitudes.xlsx")},
      content = function(file) {write_xlsx(list('Numerator'=df_Num_amplitude(), 
                                                'Denominator'=df_Den_amplitude(), 
                                                'Ratio' = df_ratio_amplitude(), 
                                                'custom_ratio' = df_custom_ratio_amplitude(),
                                                'Num_summary' = df_Num_summary(),
                                                'Den_summary' = df_Den_summary(),
                                                'Ratio_summary' = df_ratio_summary(),
                                                'Custom_ratio_summary' = df_custom_ratio_summary()
                                                ), path = file)}
    )
    
    
    

# Shifting curves / box 1 ---------------------------------------------------------

    
    # Getting the list of sheets in excel file
    sheets_in_the_file = eventReactive(eventExpr = {input$read_sheets},
                                       valueExpr = {
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
      
      valueExpr = {
        req(input$read_sheets, input$sheets)
        
        
        read_excel(input$read_sheets$datapath, sheet = input$sheets)})
    
    output$dt_to_shift_out <- DT::renderDataTable({
      req(input$read_sheets)
      req(input$sheets)
      customDT(dt_to_shift(), scrollY = '200px')
    }) 
    
    # Resulting dataframe with shifted/not shifted values
    reactive_df_to_shift <- reactiveVal(NULL)
    
    observeEvent(dt_to_shift(), {

      updateSelectInput(inputId = "cellShiftInput",
                        choices = colnames(time_col_name(dt_to_shift(), name_only = T))[-1],
                        selected = colnames(time_col_name(dt_to_shift(), name_only = T))[2])
      
      reactive_df_to_shift(dt_to_shift())
    }) 
    
    
# Shifting curves / box 2 -------------------------------------------------

    # Rendering initial plot single
    observeEvent(input$plots_init_single, {
      output$plot_shift_upper <- renderPlotly({
        req(input$sheets,
            input$cellShiftInput,
            input$start_t_shift,
            input$end_t_shift)
        ggplotly_render(display_single_plot(dt_to_shift(), 
                                    input$cellShiftInput, 
                                    ready = F), 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
        })
    })
    
    # Rendering initial plot all
    observeEvent(input$plots_init_all, {
      output$plot_shift_upper <- renderPlotly({
        req(input$sheets)
        ggplotly_render(dt_to_shift(), 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift,
                        rcolor = color_palette(dt_to_shift(), rmcellValues$colors2000))})
    }) 
    
    
# Shifting curves-----------------------------------------------------------------------
    
    
    
    observeEvent(input$shift_reset, {
      req(dt_to_shift())
      reactive_df_to_shift(dt_to_shift())
      lag_values_df(NULL)
      
      runjs('
            document.getElementById("shift_reset").style.backgroundColor = "green";
            document.getElementById("shift_maximum").style.backgroundColor = "";
            document.getElementById("shift_curves").style.backgroundColor = "";
            ')

      
      # runjs('
      #       var otherButtonColor1 = document.getElementById("shift_maximum").style.backgroundColor;
      #       
      #       if (otherButtonColor1 === "red") {
      #           document.getElementById("shift_maximum").style.backgroundColor = "";
      #           }
      #       ')
      # 
      # runjs('
      #       var otherButtonColor2 = document.getElementById("shift_curves").style.backgroundColor;
      # 
      #       if (otherButtonColor2 === "red" ) {
      #           document.getElementById("shift_curves").style.backgroundColor = "";
      #           }
      #       ')
      
    })
    
    
    
    # Lag values datatable and shifting using CCF
    lag_values_df <- reactiveVal(NULL)

    observeEvent(input$shift_curves, {
      req(input$read_sheets)
      req(input$sheets)
      
      runjs('
            var otherButtonColor = document.getElementById("shift_maximum").style.backgroundColor;
            if (otherButtonColor === "green") {
            
                document.getElementById("shift_maximum").style.backgroundColor = "red";
                document.getElementById("shift_curves").style.backgroundColor = "red";
                
            } else {
            
                document.getElementById("shift_curves").style.backgroundColor = "green";
            }
            
            document.getElementById("shift_reset").style.backgroundColor = "";
                
            ')
      

      
      lag_values_df(CCF_matrix(reactive_df_to_shift(), 
                               lower = input$start_t_shift,
                               upper = input$end_t_shift,
                               max_lag = input$max_lag))
      
      shifted_df <- eventReactive(input$plots_shift_omit, {
        shifted_result <- 
        shift_with_CCF(dt_to_shift(), 
                       lag_values_df(), 
                       max_lag = input$max_lag)
          
          if(input$plots_shift_omit == T) {
            shifted_result <- na.omit(shifted_result)
            return(shifted_result)
          } else {return(shifted_result)}
          
      }, ignoreNULL = FALSE)
      
      reactive_df_to_shift(shifted_df())
      
      output$lag_values_df_out <- DT::renderDataTable({
        req(input$read_sheets)
        req(input$sheets)
        lag_values_df()
      }) 
          
    }) 
    
    observeEvent(input$shift_maximum, {
      req(input$read_sheets)
      req(input$sheets)
      
      runjs('
              var otherButtonColor = document.getElementById("shift_curves").style.backgroundColor;
              if (otherButtonColor === "green") {
              
                  document.getElementById("shift_maximum").style.backgroundColor = "red";
                  document.getElementById("shift_curves").style.backgroundColor = "red";
                
              } else {
              
                  document.getElementById("shift_maximum").style.backgroundColor = "green";
                
              }
              
              document.getElementById("shift_reset").style.backgroundColor = "";
              
            ')
  
      shifted <- shiny::isolate(reactive_df_to_shift())
      
      lag_values_df(finding_local_maximum(shifted, 
                                          input$response_window))
      
      shifted_df <- eventReactive(input$plots_shift_omit, {
        
        shifted_result <- shift_to_match_maximum(shifted, lag_values_df())
        
        if(input$plots_shift_omit == T) {
          shifted_result <- na.omit(shifted_result)
          return(shifted_result)
        } else {return(shifted_result)}
        
        
        }, ignoreNULL = FALSE)
      
      reactive_df_to_shift(shifted_df())
      
      output$lag_values_df_out <- DT::renderDataTable({
        req(input$read_sheets)
        req(input$sheets)
        as.data.frame(lag_values_df())
      }) 
      
      
    }) 

    
    
    
    # Rendering lower plot
    observeEvent(input$plots_shift_single, {
      
      output$plot_shift_lower <- renderPlotly({
        req(input$sheets)
        ggplotly_render(display_single_plot(shifted_dataframe(), 
                                            input$cellShiftInput,
                                            ready = F), 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift)
        })
      
      
    })
    
    observeEvent(input$plots_shift_all, {
      
      output$plot_shift_lower <- renderPlotly({
        req(input$sheets)
        ggplotly_render(shifted_dataframe(), 
                        region = T, 
                        r_min = input$start_t_shift, 
                        r_max = input$end_t_shift,
                        rcolor = color_palette(shifted_dataframe(), rmcellValues$colors2000))
        }) 

    
      
    }) 
    
    shifted_dataframe <- eventReactive(eventExpr = {
      input$plots_shift_omit
      input$plots_shift_all
      input$plots_shift_single
      input$shift_maximum
      input$shift_reset
      input$plots_init_all
      input$plots_init_single
      input$shift_curves}, 
      
      valueExpr = {
      
      shifted_result <- reactive_df_to_shift()
      
      if(input$plots_shift_omit == T) {
        shifted_result <- na.omit(shifted_result)
        return(shifted_result)
      } else {return(shifted_result)}
      
      
    }, ignoreNULL = FALSE)
    
    # Save SHIFTED curves as excel file
    

# DEBUGGING ---------------------------------------------------------------
    output$read_sheets_value_out <- renderPrint({input$plots_shift_omit})
    
    
    
    output$SavePltsShift <- downloadHandler(
      filename = function() {filename(input$read_sheets$name, "Shifted.xlsx")},
      content = function(file) {
        
        wb <- createWorkbook()
        
          sheet1 <- addWorksheet(wb, sheetName = input$sheets)
          writeDataTable(wb, sheet1, shifted_dataframe())
          sheet2 <- addWorksheet(wb, sheetName = paste0(input$sheets, '_lagMatrix'))
          writeDataTable(wb, sheet2, as.data.frame(lag_values_df()))
        
        saveWorkbook(wb, file)

        }
    )
    

# Shifting curves / Box 3 ----------------------------------------------------

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
    
    
    
# Single PLOT -------------------------------------------------------------
    
    
    dataframe_to_process <- reactiveVal(NULL)
    
    
    
    
    # Plotting single graph and rotate
    
    observeEvent(ignoreInit = TRUE, list(
      input$plot_single_to_rotate,
      input$cell_to_plot_to_rotate
      ), {
      
      

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
      
      
    }) # /level 1, observeEvent input$plot_single_to_rotate  
    
    
    
    
    
    
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
      req(df2dim_single_and_rotated())
      dataframe_to_process_new <- replace_columns_in_dfs(dataframe_to_process(), df2dim_single_and_rotated())
      dataframe_to_process(dataframe_to_process_new)
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
      
      req(df2dim_single_and_rotated_part())
      dataframe_to_process_new2 <- replace_columns_in_dfs(dataframe_to_process(), df2dim_single_and_rotated_part())
      dataframe_to_process(dataframe_to_process_new2)
      
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
    
    

    
    observeEvent(input$render_rotated_single_plot, {

      
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
      
      
      ggplotly(plot)  })
    
      
      }) # observeEvent(input$render_rotated_single_plot)
    

    
    
# Button to obtain list of dataframes that have been changed manually -----------------------------    
    
    
    
    
    # Buttons to Exclude/Undo/Include/Reset cells
    
    observeEvent(ignoreInit = TRUE, list(
      input$rotate_single_plot, input$rotate_single_plot_part), {
        
        req(data_to_rotate())
        
        
        rmcellValues$cells_altered_manually <- 
          unique(c(shiny::isolate(rmcellValues$cells_altered_manually), 
                   shiny::isolate({finding_cell_name(data_to_rotate(), 
                                                     input$cell_to_plot_to_rotate)})))
        
        
        
        output$list_of_cells_altered_manually<-renderPrint({
          gtools::mixedsort(rmcellValues$cells_altered_manually, decreasing = F)
          
        })
      }) # /level 1, observeEvent input$rotate_single_plot | input$rotate_single_plot_part
    
    
    
    observeEvent(input$reset_current_changes, {
      
      dataframe_to_process_new <- 
        replace_columns_in_dfs(dataframe_to_process(),
                               getting_a_slice_of_df(data_to_rotate(), 
                                                     input$cell_to_plot_to_rotate))
      
      dataframe_to_process(dataframe_to_process_new)
      
      req(rmcellValues$cells_altered_manually)
      rmcellValues$cells_altered_manually <- 
        rmcellValues$cells_altered_manually[-grep(finding_cell_name(data_to_rotate(),
                                                                    input$cell_to_plot_to_rotate),
                                                  rmcellValues$cells_altered_manually)]
      
      
      output$list_of_cells_altered_manually<-renderPrint({
        gtools::mixedsort(rmcellValues$cells_altered_manually, decreasing = F)
      })
    }) # /level 1, observeEvent input$reset_last_changes
    
    
    
    observeEvent(input$reset_last_changes, {
      
      dataframe_to_process_new <- 
        replace_columns_in_dfs(dataframe_to_process(), 
                               getting_a_slice_of_df(data_to_rotate(), 
                                                     rmcellValues$cells_altered_manually[length(rmcellValues$cells_altered_manually)], c_name = T))
      
      dataframe_to_process(dataframe_to_process_new)
      
      req(rmcellValues$cells_altered_manually)
      rmcellValues$cells_altered_manually <- 
        rmcellValues$cells_altered_manually[-length(rmcellValues$cells_altered_manually)]
      
      
      output$list_of_cells_altered_manually<-renderPrint({
        gtools::mixedsort(rmcellValues$cells_altered_manually, decreasing = F)
      })
    }) # /level 1, observeEvent input$reset_last_changes
    
    
    
    
    observeEvent(input$reset_all_changes, {
      
      dataframe_to_process(data_to_rotate())
      
      rmcellValues$cells_altered_manually <- c()
      
      
      output$list_of_cells_altered_manually<-renderPrint({
        rmcellValues$cells_altered_manually
      })
      
      
    }) # /level 1, observeEvent input$reset_all_changes
    
    
    

# Rotating every cell's plot one by one -----------------------------------

    observeEvent(input$rotate_all_other_cells, {
      
      baselinev <- shiny::isolate(input$rotate_baseline_as_well)
      shiftv <- shiny::isolate(input$rotate_single_down)
      
      dataframe_to_process_new2 <- rotate_all(dataframe_to_process(), 
                                              rmcellValues$cells_altered_manually, 
                                              lower_base = input$line_start, 
                                              upper_base = input$line_end,
                                              lower_reg = input$flat_start,  
                                              upper_reg = input$flat_end, 
                                              baseline_r = input$rotate_baseline_as_well,
                                              shift_down = input$rotate_single_down)
      
      dataframe_to_process(dataframe_to_process_new2)
      
      
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
        
        
        ggplotly(plot)  })
      
      output$list_of_cells_altered_manually<-renderPrint({
        

        if ((baselinev != input$rotate_baseline_as_well) | (shiftv != input$rotate_single_down)) {
          
          text <- ("Press 'Reset all' button to recalculate!")
          print(text)
          
        } else {gtools::mixedsort(rmcellValues$cells_altered_manually, decreasing = F)}
        
        
        
      })
      
      

    })    
    
    
    
    observeEvent(input$rotate_all_cells_from_scratch, {

      rmcellValues$cells_altered_manually <- c()

      baselinev <- shiny::isolate(input$rotate_baseline_as_well)
      shiftv <- shiny::isolate(input$rotate_single_down)

      dataframe_to_process_new2 <- rotate_all(data_to_rotate(),
                                              rmcellValues$cells_altered_manually,
                                              lower_base = input$line_start,
                                              upper_base = input$line_end,
                                              lower_reg = input$flat_start,
                                              upper_reg = input$flat_end,
                                              baseline_r = input$rotate_baseline_as_well,
                                              shift_down = input$rotate_single_down)

      dataframe_to_process(dataframe_to_process_new2)


      output$list_of_cells_altered_manually<-renderPrint({

        text <- ("All curves were rotated automatically: ")

        if (baselinev) {
          text <- paste0(text, '(main region + baseline)')
        } else {
          text <- paste0(text, '(main region only)')
        }

        if (shiftv) {
          text <- paste0(text, ' and baseline shifted to match the next value')
        } else {
          text <- paste0(text, ' and baseline is not shifted')
        }

        if ((baselinev != input$rotate_baseline_as_well) | (shiftv != input$rotate_single_down)) {

          text <- ("Press button to rotate all the rest or from scratch!")

        }

        print(text)
      })




      output$data_to_rotate_out2 <- DT::renderDataTable({
        req(data_to_rotate())
        dataframe_to_process()
      })



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


        ggplotly(plot)  })


    })
    
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
        
        pattern <- grep('([Tt]ime\\s?)', colnames(dataframe_to_process()))
        
        
        # dataframe_to_process <- cbind(dataframe_to_process()[pattern], 
        #                               data.frame(lapply(dataframe_to_process()[-pattern], decim)))
        
        
        dataframe_to_process <- cbind(dataframe_to_process()[pattern], 
                                      dcm((dataframe_to_process()[-pattern])))
        
        # dataframe_to_process <- data.frame(lapply(dataframe_to_process()[-pattern], decim))
        
        
        df_list <- list('first' = dataframe_to_process,
                        'second' = average_curve(dataframe_to_process()),
                        'third' = data_to_rotate(),
                        'forth' = average_curve(data_to_rotate()),
                        'fifth' = plot_average_part()
                        )
        
        names(df_list) <- c(paste0(input$sheets, 'data_rotated'), 
                            paste0(input$sheets, '_av_rotated'), 
                            paste0(input$sheets, 'data_initial'), 
                            paste0(input$sheets, '_av_initial'),
                            paste0(input$sheets, '_av_rotated_initial'))
        write_xlsx(df_list, path = file)
        
      }
    )
    

# Rotating plot / box 2 - Plot rotated result -----------------------------

    
    
    
    
    # Render plot
    
    
    plot_average_output2 <- eventReactive(eventExpr = {input$plot_rotated_result
      input$rotate_average
      input$rotate_part
      input$rotate_down
      input$baseline_start 
      input$baseline_end 
      input$area_start
      input$area_end      
      
      },

      valueExpr = {
        
        req(dataframe_to_process())
        
        baseline <- input$mark_line_to_rotate
        
        curve <- ggplotly_render(average_curve(dataframe_to_process()), 
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
    
    
    

    

# Calculating area on a plot ----------------------------------------------

    df_slice <- eventReactive(eventExpr = {
      input$rotated_plots
      input$mark_line_to_calculate
      input$render_rotated_plots
      input$baseline_start
      input$baseline_end
      input$area_start
      input$area_end
      
    }, valueExpr = {
      
      df_slice <- getting_a_slice_of_df(dataframe_to_process(), input$rotated_plots)

      return(df_slice)
    })
    
    
    b_value <- eventReactive(eventExpr = {
      input$rotated_plots
      input$mark_line_to_calculate
      input$render_rotated_plots
      input$baseline_start
      input$baseline_end
      input$area_start
      input$area_end
      
    }, valueExpr = {
      
      df_slice <- df_slice()
      
      b_value <- b_find(df_slice, input$baseline_start, input$baseline_end)
      
      return(b_value)
    })

    
    intersections_df <- eventReactive(eventExpr = {
      input$rotated_plots
      input$mark_line_to_calculate
      input$render_rotated_plots
      input$baseline_start
      input$baseline_end
      input$area_start
      input$area_end
      
      }, valueExpr = {
        
      b <- b_value()

      dfres <- dataframe_with_intersections(df_slice(), input$area_start, input$area_end, b)
      
      return(dfres)
      })
    
    
    polygon_df <- eventReactive(eventExpr = {intersections_df()
                                             input$area_start
                                             input$area_end
                                             b_value()}, 
                                valueExpr = {
  
      b <- b_value()
      
      dfres <- polygon_function(intersections_df(), input$area_start, input$area_end, b)
      
      return(dfres)
    })
    
    


    
    observeEvent(ignoreInit = TRUE, list(
      input$render_rotated_plots,
      input$mark_line_to_calculate,
      input$rotated_plots,
      input$baseline_start,
      input$baseline_end,
      input$area_start,
      input$area_end), {
        
        
        #  Calculate area is:

        output$area_value <- renderPrint({
          
          auc_upper <-  AUC(intersections_df()[[1]], 
                            polygon_df()[[2]], 
                            from = input$area_start, 
                            to = input$area_end, 
                            method = "trapezoid")
          
          auc_lower <- AUC(intersections_df()[[1]], 
                           rep(b_value(), length.out = nrow(polygon_df())), 
                           from = input$area_start, 
                           to = input$area_end, 
                           method = "trapezoid")
          
          auc <- auc_upper - auc_lower
          
          return(decim(auc))
        })
        
        

        output$plot_single_area_out <- renderPlotly({
          req(input$render_rotated_plots, polygon_df(), input$render_rotated_plots)

          
          df_slice <- getting_a_slice_of_df(dataframe_to_process(), input$rotated_plots)
          b <- b_find(df_slice, input$baseline_start, input$baseline_end)
          pl <- polygon_df()
          plot <- ggplotly_render(intersections_df(),
                                  baseline = input$mark_line_to_rotate,
                                  b_min = input$baseline_start,
                                  b_max = input$baseline_end,
                                  region = input$mark_line_to_rotate,
                                  r_min = input$area_start,
                                  r_max = input$area_end,
                                  ready = FALSE) +
                        scale_color_manual(values='black')

          if (input$mark_line_to_calculate[[1]] %% 2 == 1) {


            plot <- plot +
              geom_point(size = 2) +
              geom_hline(yintercept=b, color = "blue") +
              geom_polygon(mapping=aes(pl[[1]],
                                       pl[[2]]),
                           fill = 'green') +
              geom_point(mapping=aes(pl[[1]],
                                     pl[[2]]),
                         size = 1,
                         colour = "red")

          }


      ggplotly(plot)  }) # output$plot_single_area_out



    }, ignoreNULL = FALSE) # observeEvent(ignoreInit = TRUE, list(

 

# Calculating area values -------------------------------------------------

       
    
    all_curves_area <- 
      eventReactive(eventExpr = {input$calculate_area},
                    valueExpr = {
                      
                      area_df <- polygon_df() %>% 
                        pivot_longer(!Time, names_to = "cell", values_to = "value")
                      
                      result_area_df <- summarize(group_by(area_df, cell),
                                        Area_under_the_curve = (AUC(x = Time, 
                                                                    y = value, 
                                                                    from = input$area_start, 
                                                                    to = input$area_end, 
                                                                    method = "trapezoid")-
                                                                  AUC(x = Time, 
                                                                      y = b_value(),
                                                                      from = input$area_start, 
                                                                      to = input$area_end, 
                                                                      method = "trapezoid")
                                                                )
                                                  
                                                  
                                                  )
                      
                      return(result_area_df)
                                })
    
    
    # ALL_POLYGONS_DF ---------------------------------------------------------
    
    
    all_polygons_df <-
      eventReactive(eventExpr = {input$calculate_area},
                    valueExpr = {
                      
                      rotated_df <- dataframe_to_process()
                      
                      idx_c <- c()
                      res_table <- data.frame(Cell = character(), 
                                              Area = numeric(), 
                                              Difference = numeric(),
                                              Area_by_Dif = numeric(),
                                              Maximum = numeric(),
                                              Baseline_average = numeric(),
                                              SD_baseline = numeric(),
                                              CV_baseline_pct = numeric()
                      )
                      
                      for (idx in 2:ncol(rotated_df)) {
                        
                        df_slice <- rotated_df[, c(1, idx)]
                        b <- b_find(df_slice, input$baseline_start, input$baseline_end)
                        intersections_df <- dataframe_with_intersections(df_slice, input$area_start, input$area_end, b)
                        dfres <- polygon_function(intersections_df, input$area_start, input$area_end, b)
                        dfres[, 1] <- intersections_df[, 1]
                        
                        auc_upper <-  AUC(dfres[[1]], 
                                          dfres[[2]], 
                                          from = input$area_start, 
                                          to = input$area_end, 
                                          method = "trapezoid")
                        
                        auc_lower <- AUC(dfres[[1]], 
                                         rep(b, length.out = nrow(dfres)), 
                                         from = input$area_start, 
                                         to = input$area_end, 
                                         method = "trapezoid")
                        
                        auc <- auc_upper - auc_lower
                        
                        sd = SD(subset(df_slice, (Time >= input$baseline_start) & (Time <= input$baseline_end))[[2]])
                        cv = (sd / b)*100
                        maximum = max(subset(df_slice, (Time >= input$area_start) & (Time <= input$area_end))[[2]])
                        difference <- maximum - b
                        
                        area_by_dif <- auc/difference
                        
                        res_table[nrow(res_table)+1, ] <- 
                          c(Cell = colnames(rotated_df)[idx], 
                            Area = decim(auc), 
                            Difference = decim(difference), 
                            Area_by_Dif = decim(area_by_dif),
                            Maximum = decim(maximum),
                            Baseline_average = decim(b),
                            SD_baseline = decim(sd),
                            CV_baseline_pct = decim(cv, 1)
                          )
                        
                      }
                      
                      
                      return(res_table)
                    })
    
    result_table <- reactiveVal(NULL)
    
    observeEvent(input$calculate_area, {
      
      output$area_data_out <- renderDataTable({
        req(all_polygons_df())
        result_table(all_polygons_df())
        result_table_n <- result_table()
        for (n in 2:ncol(result_table_n)) {
          result_table_n[n] <- as.numeric(result_table_n[[n]])
        }
        return(result_table_n)
      })
      
    }
    ) # observeEvent(input$calculate_area, {
    
    
    observeEvent(input$save_area_single, {
      
      
      single_polygon_df <-
        eventReactive(eventExpr = {input$calculate_area},
                      valueExpr = {
                        
                        if (!is.null(result_table())) {
                          df_slice <- df_slice()
                          b <- b_value()
                          res_table <- all_polygons_df()
                          
                          auc_upper <-  AUC(intersections_df()[[1]],
                                            polygon_df()[[2]],
                                            from = input$area_start,
                                            to = input$area_end,
                                            method = "trapezoid")
                          
                          auc_lower <- AUC(intersections_df()[[1]],
                                           rep(b, length.out = nrow(polygon_df())),
                                           from = input$area_start,
                                           to = input$area_end,
                                           method = "trapezoid")
                          
                          auc <- auc_upper - auc_lower
                          
                          sd = SD(subset(df_slice, (Time >= input$baseline_start) & (Time <= input$baseline_end))[[2]])
                          cv = (sd / b)*100
                          maximum = max(subset(df_slice, (Time >= input$area_start) & (Time <= input$area_end))[[2]])
                          difference <- maximum - b
                          
                          res_table[which(res_table$Cell == colnames(df_slice)[2]), ] <-
                            c(Cell = colnames(df_slice)[2],
                              Area = decim(auc),
                              Difference = decim(difference),
                              Area_by_Dif = decim(auc/difference),
                              Maximum = decim(maximum),
                              Baseline_average = decim(b),
                              SD_baseline = decim(sd),
                              CV_baseline_pct = decim(cv, 1)
                            )
                        }
                        
                        return(res_table)
                      })
      
      result_table(single_polygon_df())
      
      output$area_data_out <- renderDataTable({
        req(all_polygons_df())
        result_table <- result_table()
        for (n in 2:ncol(result_table)) {
          result_table[n] <- as.numeric(result_table[[n]])
        }
        return(result_table)
      })
      
    }
    ) # observeEvent(input$save_area_single, {
    
    
    
    
    result_statistics_df <-
      eventReactive(eventExpr = {input$calculate_area_statistics},
                    
                    
                    valueExpr = {
                      
                      if (!is.null(result_table())) {
                        
                        res_table <- result_table()
                        
                        for (n in 2:ncol(res_table)) {
                          res_table[, n] <- as.numeric(res_table[, n])
                        }
                        
                        Mean_Area = mean(res_table$Area)
                        
                        
                        res_statistics <- data.frame(
                          Mean_Area = decim(mean(res_table$Area)),
                          SD_Area = decim(SD(res_table$Area)),
                          CV_Area_pct = decim(100*SD(res_table$Area)/mean(res_table$Area), 1),
                          Mean_Difference = decim(mean(res_table$Difference)),
                          SD_Difference = decim(SD(res_table$Difference)),
                          CV_Difference_pct = decim(100*SD(res_table$Difference)/mean(res_table$Difference), 1),
                          Shapiro_Wilk_area_W = decim(shapiro.test(res_table$Area)[[1]]),
                          Shapiro_Wilk_area_P = shapiro.test(res_table$Area)[[2]],
                          Shapiro_Wilk_difference_W = decim(shapiro.test(res_table$Difference)[[1]]),
                          Shapiro_Wilk_difference_P = shapiro.test(res_table$Difference)[[2]],
                          Mean_A_by_d = decim(mean(res_table$Area_by_Dif)),
                          CV_A_by_d_pct = decim(100*SD(res_table$Area_by_Dif)/mean(res_table$Area_by_Dif), 1),
                          Mean_Baseline_av = decim(mean(res_table$Baseline_average)),
                          CV_Baseline_av_pct = decim(100*SD(res_table$Baseline_average)/mean(res_table$Baseline_average), 1)
                        )
                        


                      }
                      
                      return(res_statistics)
                      
                    })
    
    output$result_statistics_out <- renderDataTable({
      req(input$calculate_area_statistics,
          result_statistics_df())
      
      result_statistics_df()})
    
    
# Saving data with area statistics -------------------------------------------------
    
    output$SaveAreaStatistics <- downloadHandler(
      filename = function() {filename(input$read_curves$name, "Area_Statistics.xlsx")},
      content = function(file) {
        

        
        
        df_list <- list('area' = result_table(),
                        'statistics' = result_statistics_df(),
                        'data_rotated' = dataframe_to_process()
        )
        
        
        write_xlsx(df_list, path = file)
        
      }
    )       
    
} # level 0