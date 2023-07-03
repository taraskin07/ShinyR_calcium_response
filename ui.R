source('engine.R')


# Define UI for data upload app ----
ui <- navbarPage("Calcium response plots adjustment", theme = shinytheme("cosmo"), fluid = TRUE, position = "static-top", # 0 level


                 

 # Preliminary analysis -----------------------------------------------------                                  
                 
tabPanel("Preliminary analysis", # /level 1 - tabPanel Preliminary analysis

                  
# Panel title ----
  titlePanel("Upload and save"), # 2 level - titlePanel "Upload and save"


# Preliminary analysis / box 1 --------------------------------------------


  # Sidebar layout with input and output definitions /level 2 /box 1----
  tabsetPanel(
    
    # Sidebar panel for inputs: file input and preferences
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("fluorescence", "Choose excel File",
                multiple = FALSE,
                accept = c(".xls",
                           ".xlsx")),
      
      # Horizontal line 
      tags$hr(),
      

      # Input: Checkbox if file has header ----
      checkboxInput("c340", "sheet 340", TRUE),
      checkboxInput("c380", "sheet 380", TRUE),
      checkboxInput("cRatio", "sheet Ratio", TRUE),
      
      # Horizontal line
      tags$hr(),
      
      actionButton("correct_time", "Correct 'Time' column"),
      tags$br(),
      tags$hr(),
      radioButtons("change_names", "Change columns names",
                   choices = c(No_changes = "no_changes",
                               Number_only = "number",
                               Zeroes_in_front = "zeroes"),
                   selected = "no_changes"),

      # Horizontal line 
      tags$hr(),
      
      textInput('cellName', label = 'Enter new column names', value = "cell-"),
      actionButton("change_names_button", "Change columns names"),
      
      tags$hr(),
      
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "all"),
      
      # Save as excel file
      downloadButton("SaveXlsBox1", "Save as excel file")
    
      
              )
    ), # 2 level - main layout with sidebar and tabset inside, tabsetPanel / box 1
    
  # Main panel for displaying outputs from tabsetPanel /level 2 /box 1
  mainPanel(
      # Tabs
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Ratio", DT::dataTableOutput("df_ratio")),
                  
                  tabPanel("340", DT::dataTableOutput("df_340")),

                  tabPanel("380", DT::dataTableOutput("df_380")),

                  tabPanel("Custom Ratio", DT::dataTableOutput("df_custom_ratio")),
                  )
      ), # /level 2, /box 1, mainPanel 340-380-Ratio-Custom ratio 



# Preliminary analysis / box 2 - STATISTICS--------------------------------------------

  # Sidebar layout with input and output definitions /level 2 /box 2----
  tabsetPanel(
    
    # Sidebar panel for inputs: file input and preferences
    sidebarPanel(
                    actionButton("basicStat", "Calculate basic Stat for TS"),
                    tags$hr(),

                    # Save BASIC STATISTICS as excel file
                    tags$br('Save basic statistics as excel file'),
                    downloadButton("SaveXlsBoxStat", "Save as excel file"),

    
                ) # /level 3, /box 2, sidebarPanel for statistics
                
            ), # /level 2, /box 2, tabsetPanel for statistics

# Main panel for displaying outputs from tabsetPanel /level 2 /box 2
  mainPanel(
  # Tabs
  tabsetPanel(type = "tabs",
              
              tabPanel("Ratio", DT::dataTableOutput("df_ratio_basic_stat_out")),
              
              tabPanel("340", DT::dataTableOutput("df_340_basic_stat_out")),

              tabPanel("380", DT::dataTableOutput("df_380_basic_stat_out")),


              tabPanel("Custom Ratio", DT::dataTableOutput("df_custom_ratio_basic_stat_out")),
  )
), # /level 2, /box 2, mainPanel Statistics
  


# Preliminary analysis / box 3 - plots ------------------------------------

  # Sidebar layout with input and output definitions /level 2 /box 2----
  tabsetPanel(
    # Sidebar panel for inputs: file input and preferences
    sidebarPanel(
      actionButton("plot_all", "Plot all graphs"),
      actionButton("plot_single", "Plot single graph"),
      
      tags$hr(),
      numericInput("cell_to_plot", "Enter number of cell", 1),
      
      actionButton("exclude_cell", "Exclude cell"),
      actionButton("exclude_undo", "Undo"),
      actionButton("include_cell", "Include cell"),
      actionButton("exclude_reset", "Reset"),

      tags$hr('Cells to be excluded:'),
      verbatimTextOutput("list_of_cells", placeholder = TRUE),
      
      tags$br(),
      actionButton("new_dataframes", "Obtain new tables"),
      actionButton("plot_new_all", "Plot all new graphs"),
      
      
      # Save DATA WITHOUT BAD CELLS as excel file
      tags$hr(),
      tags$br('Save new data without bad cells as excel file'),
      downloadButton("SaveXlsBoxNoBadCells", "Save as excel file"),
      
    ) # /level 3, /box 2, sidebarPanel for plots
  ), # /level 2, /box 3, tabsetPanel for plots

  # Main panel for displaying outputs from tabsetPanel /level 2 /box 3
  mainPanel(
    # Tabs
    tabsetPanel(type = "tabs",
                
                tabPanel("Ratio", plotlyOutput("plot_ratio") # /level 5, /box 3, plotlyOutput
                ), # /level 4, /box 3, tabPanel Ratio
                
                
                tabPanel("340", plotlyOutput("plot340") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel 340

                tabPanel("380", plotlyOutput("plot380") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel 380
                
                
                tabPanel("Custom Ratio", plotlyOutput("plot_custom_ratio") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel Custom Ratio
                
                ) # /level 3, /box 3, tabsetPanel for plots
            ), # /level 2, /box 3, mainPanel for plots





# Preliminary analysis / box 3 - Debugging window --------------------------------------------------------



# Main panel for displaying outputs from tabsetPanel /level 2 /box 3
mainPanel(
  # Tabs
  tabsetPanel(type = "tabs",
              tabPanel("df_340_ready_db", DT::dataTableOutput("df_340_ready_db") # 
              ), # 
              
              tabPanel("df_340_excluded_db", DT::dataTableOutput("df_340_excluded_db") # 
              ), # 0
              # 
              tabPanel("rmcellValues_cList", verbatimTextOutput("rmcellValues_cList") # 
              ), # 
              # 
              # 
              # tabPanel("Custom Ratio", plotlyOutput("") # /level 5, /box 3, plotlyOutput
              # ), # /level 4, /box 3, tabPanel Custom Ratio
              
  ) # /level 3, /box 3, tabsetPanel for plots
), # /level 2, /box 3, mainPanel for plots




  ), # 1 level - tabPanel Preliminary analysis














# Analyzing amplitude -----------------------------------------------------

tabPanel("Analyzing amplitude", # /level 1 - tabPanel Analyzing amplitude
         
         # Analyzing amplitude sidebar title ----
         titlePanel("Upload clean excel file"), # 2 level - titlePanel "Upload clean excel file"
         

         
# Analyzing amplitude / box 1 --------------------------------------------

# Sidebar layout with input and output definitions /level 2 /box 1----
tabsetPanel(
  
  # Sidebar panel for inputs: file input and preferences
  sidebarPanel(
    
    # Input: Select a file ----
    fileInput("clean_file", "Choose excel File with 'clean' data",
              multiple = FALSE,
              accept = c(".xls",
                         ".xlsx")),
    
    # Horizontal line 
    tags$hr(),
    
    
    # Input: Checkbox if the file has a header ----
    checkboxInput("cl340", "sheet 340", TRUE),
    checkboxInput("cl380", "sheet 380", TRUE),
    checkboxInput("clRatio", "sheet Ratio", TRUE),
    
  )
), # Analyzing amplitude /level 2 /box 1  - main layout with sidebar and tabset inside, tabsetPanel / box 1

# Main panel for displaying outputs from tabsetPanel /level 2 /box 1
mainPanel(
  # Tabs
  tabsetPanel(type = "tabs",
              
              tabPanel("Ratio", DT::dataTableOutput("cl_ratio")),
              
              tabPanel("340", DT::dataTableOutput("cl_340")),
              
              tabPanel("380", DT::dataTableOutput("cl_380")),
              
              tabPanel("Custom Ratio", DT::dataTableOutput("cl_custom_ratio")),
  )
), # Analyzing amplitude /level 2 /box 1, mainPanel 340-380-Ratio-Custom ratio 



# Analyzing amplitude / box 2 - STATISTICS for CLEAN DATA ----------------


# Sidebar layout with input and output definitions - Analyzing amplitude /level 2 /box 2
tabsetPanel(
  
  # Sidebar panel for inputs: file input and preferences
  sidebarPanel(
    tags$br("Enter timeframe for the baseline and response"),
    tags$hr(),
    numericInput("min_time", "Baseline START: (sec)", 0),
    numericInput("max_time", "Baseline END: (sec)", 120),
    tags$hr(),
    numericInput("start_time", "Region of interest START: (sec)", 150),
    numericInput("end_time", "Region of interest END: (sec)", 330),
    tags$hr(),
    actionButton("amplitudeStat", "Calculate amplitudes"),
    tags$br(),
    tags$hr(),
    
    # Save AMPLITUDES as excel file
    tags$br('Save amplitudes info in excel file'),
    downloadButton("SaveXlsAmpl", "Save as excel file"),
    
    
  ) # /level 3, /box 2, sidebarPanel for statistics
  
), # /level 2, /box 2, tabsetPanel for statistics


# Main panel for displaying outputs from tabsetPanel Analyzing amplitude /level 2 /box 2
mainPanel(
  # Tabs
  tabsetPanel(type = "tabs",
              
              tabPanel("Ratio", DT::dataTableOutput("df_ratio_amplitude_out")),
              
              tabPanel("340", DT::dataTableOutput("df_340_amplitude_out")),
 
              tabPanel("380", DT::dataTableOutput("df_380_amplitude_out")),

              tabPanel("Custom Ratio", DT::dataTableOutput("df_custom_ratio_amplitude_out")),
  )
), # /level 2, /box 2, mainPanel Analyzing amplitude

  ), # 1 level - tabPanel Analyzing amplitude







# TAB PANEL Shifting curves ---------------------------------------------------------


tabPanel("Shifting curves", # /level 1 - tabPanel Shifting curves
         
         # Panel title ----
         titlePanel("Shifting ratio plot"), # 2 level - titlePanel "Shifting ratio plot"


# Shifting curves / box 1 -------------------------------------------------


         # Sidebar panel for inputs: file input and preferences

sidebarLayout(
         sidebarPanel(
                      
                      # Input: Select a file ----
                      fileInput("read_sheets", "Choose excel File with Ratio sheet",
                                multiple = FALSE,
                                accept = c(".xls",
                                           ".xlsx")),
                      
                      # Horizontal line 
                      tags$hr(),
                      
                      selectInput('sheets', 'Select the sheet', '', selected = '', multiple = FALSE),
                      
                      
                      
                      
                      
                      ), # 3 level - main layout with sidebar, sidebarPanel - Shifting curves / box 1
         
         
         mainPanel(
           DT::dataTableOutput("dt_to_shift_out")
           
           ), # 3 level - mainPanel - Shifting curves / box 1
         
position = 'left'), # 2 level - sidebarLayout - Shifting curves / box 1



# Shifting curves / box 2 -------------------------------------------------


          # Sidebar panel for plots

sidebarLayout(
          sidebarPanel(style = "height: 100%",
  
                      tags$br("Enter timeframe for the baseline and region to analyze"),
                      tags$hr(),
                      numericInput("min_t_shift", "Baseline START: (sec)", 0),
                      numericInput("max_t_shift", "Baseline END: (sec)", 120),
                      tags$hr(),
                      numericInput("start_t_shift", "Region to analyze START: (sec)", 0),
                      numericInput("end_t_shift", "Region to analyze END: (sec)", 500),
                      tags$hr(),
                      numericInput("cell_to_plot_shift", "Enter number of cell", 1),
                      tags$hr(),
                      actionButton("plots_init_single", "Render single plot", width = "100%"),
                      tags$br(),
                      tags$br(),
                      actionButton("plots_init_all", "Render all plots", width = "100%"),
                      tags$hr(),
                      numericInput("max_lag", "Enter maximum lag", 40),
                      tags$hr(),
                      actionButton("shift_curves", "Shift the curves using CCF", width = "100%"),
                      tags$hr(style= 'border-style: inset;'),
                      actionButton("plots_shift_single", "Render single shifted plot", width = "100%"),
                      tags$br(),
                      tags$br(),
                      actionButton("plots_shift_all", "Render all shifted plots", width = "100%"),
                      tags$hr(style= 'border-style: inset;'),
                      actionButton("plots_shift_omit", "Omit NA values in columns", width = "100%"),
                      
                      # Save SHIFTED curves as excel file
                      tags$br(),
                      tags$br('Save shifted curves as excel file'),
                      verbatimTextOutput("read_sheets_value_out", placeholder = TRUE),
                      downloadButton("SavePltsShift", "Save as excel file"),
  
  
  
  
                      ), # 3 level - main layout with sidebar, sidebarPanel - Shifting curves / box 2


          mainPanel(
            plotlyOutput("plot_shift_upper"),
            plotlyOutput("plot_shift_lower"),
            DT::dataTableOutput("lag_values_df_out"),
            

                    ), # 3 level - mainPanel - Shifting curves / box 2

position = 'left'), # 2 level - sidebarLayout - Shifting curves / box 2



          # Sidebar panel for plots with average


sidebarLayout(
  sidebarPanel(style = "max-height: 100%",
    
    tags$br("Calculate average for shifted and initial data"),
    tags$hr(),
    actionButton("plots_average_init", "Render initial average", width = "100%"),
    tags$hr(),
    actionButton("plots_average_shifted", "Render shifted average", width = "100%"),

    
    # Save SHIFTED curves as excel file
    tags$hr('Save shifted and initial curves as excel file'),
    # verbatimTextOutput("read_sheets_value_out", placeholder = TRUE),
    downloadButton("SaveAverage", "Save as excel file"),
    
    
    
    
  ), # 3 level - main layout with sidebar, sidebarPanel - Shifting curves / box 3
  
  
  mainPanel(
    plotlyOutput("plot_average_upper"),
    plotlyOutput("plot_average_lower"),
    DT::dataTableOutput("average_values_df_out"),
    
    
  ), # 3 level - mainPanel - Shifting curves / box 3
  
  position = 'left'), # 2 level - sidebarLayout - Shifting curves / box 3




                  
), # /level 1 - tabPanel Shifting curves





# TAB PANEL Rotating plot -------------------------------------------------



tabPanel("Rotating plot", # /level 1 - tabPanel Rotating plot
         
         
  
         # Panel title ----
         titlePanel("Upload data to rotate"), # 2 level - titlePanel "Shifting ratio plot"
  
  
  
# Rotating plot / box 1 -------------------------------------------------
         
         
         # Sidebar panel for inputs: file input and preferences
         
         sidebarLayout(
           sidebarPanel(
             
             # Input: Select a file ----
             fileInput("read_curves", "Choose excel File",
                       multiple = FALSE,
                       accept = c(".xls",
                                  ".xlsx")),
             
             # Horizontal line 
             tags$hr(),
             
             selectInput("data_sheets", 'Select the sheet in file', '', selected = '', multiple = FALSE),
             
             
             
             
             
           ), # 3 level - main layout with sidebar, sidebarPanel - Rotating plot / box 1
           
           
           mainPanel(
             DT::dataTableOutput("data_to_rotate_out")
             
           ), # 3 level - mainPanel - Rotating plot / box 1
           
           position = 'left'), # 2 level - sidebarLayout - Rotating plot / box 1
  
  
  
  
  

# Rotating plot / box 2 - Visualization ------------------------------------


  

        # Sidebar panel for plots

sidebarLayout(
  sidebarPanel(style = "height: 100%",
               
               tags$br("Enter timeframe for the line to rotate and region to calculate area"),
               tags$hr(),
               actionButton("render_plot_with_average", "Render average plot", width = "100%"),
               
               
               
               
               tags$br(),
               tags$br(),
               switchInput(inputId = "rotate_average",
                           label = "Rotate the whole plot",
                           value = FALSE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", onLabel = "Rotate",
                           offLabel = "OFF", labelWidth = "100000px"),
               switchInput(inputId = "rotate_part",
                           label = "Rotate the chosen part of the plot",
                           value = FALSE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", onLabel = "Rotate",
                           offLabel = "OFF", labelWidth = "100000px"),
               switchInput(inputId = "rotate_down",
                           label = "Shift rotated part downwards",
                           value = TRUE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", onLabel = "Rotate",
                           offLabel = "OFF", labelWidth = "100000px"),
               actionButton("reset_plot", "Reset plot to initial", width = "100%"),
               
               
               
               tags$hr(),
               switchInput(inputId = "mark_line_to_rotate",
                           label = "Mark lines",
                           value = TRUE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", labelWidth = "100000px"),
               
               numericInput("line_start", "Line to rotate START: (sec)", 0),
               numericInput("line_end", "Line to rotate END: (sec)", 120),
               tags$br(),
               numericInput("flat_start", "Plot to rotate START: (sec)", 200),
               numericInput("flat_end", "Plot to rotate END: (sec)", 500),

               
               
               
               # Plotting single graph and rotate
               tags$hr(),
               numericInput("cell_to_plot_to_rotate", "Enter number of cell", 1),
               actionButton("plot_single_to_rotate", "Plot single graph (Remove unsaved manipulations)", width = "100%"),
               
               tags$br(),
               tags$br(),
               
               actionButton("rotate_single_plot", "Rotate single plot"),
               actionButton("rotate_single_plot_part", "Rotate the part of the single plot"),
               switchInput(inputId = "rotate_single_down",
                           label = "Shift down",
                           value = TRUE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", onLabel = "Shift",
                           offLabel = "OFF"),
               tags$br(),
               tags$br(),
               
               actionButton("render_rotated_single_plot", "Render rotated single plot"),
               tags$br(),
               tags$br(),
               actionButton("reset_current_changes", "Reset changes to current cell number"),
               actionButton("reset_last_changes", "Reset last changes"),
               actionButton("reset_all_changes", "Reset all"),

               tags$hr('Cells, that are changed manually:'),
               verbatimTextOutput("list_of_cells_altered_manually", placeholder = TRUE),
               
               
               tags$hr(),
               actionButton("rotate_all_other_cells", "Rotate all other cells"),
               actionButton("rotate_all_cells_from_scratch", "Rotate all cells from scratch"),
               switchInput(inputId = "rotate_baseline_as_well",
                           label = "Rotate baseline as well",
                           value = TRUE, size = "normal", onStatus = "statusON",
                           offStatus = "statusOFF", onLabel = "Rotate",
                           offLabel = "OFF"),
               

               

               # actionButton("exclude_cell", "Exclude cell"),
               # actionButton("exclude_undo", "Undo"),
               # actionButton("include_cell", "Include cell"),
               # actionButton("exclude_reset", "Reset"),
               # 
               # tags$hr('Cells to be excluded:'),
               # verbatimTextOutput("list_of_cells", placeholder = TRUE),
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               # Save SHIFTED curves as excel file
               tags$br(),
               tags$br('Save final curve as excel file'),
               downloadButton("SaveFinal", "Save as excel file"),
               
               
               tags$hr(style= 'border-style: inset;'),
               
               actionButton("plot_rotated_result", "Plot rotated result", width = "100%"),

               tags$hr('Define the baseline timeline and the region with maximum'),
               tags$br(),
               numericInput("baseline_start", "Baseline START: (sec)", 0),
               numericInput("baseline_end", "Baseline END: (sec)", 120),
               tags$hr(),
               numericInput("area_start", "Calculate area START: (sec)", 560),
               numericInput("area_end", "Calculate area END: (sec)", 760),
               tags$hr(),
               actionButton("mark_line_to_calculate", "Mark area", width = "100%"),
               tags$br(),
               tags$br(),
               actionButton("calculate_area", "Calculate area under the curve", width = "100%"),
               tags$br('The calculated area is:'),
               verbatimTextOutput("area_value", placeholder = TRUE),
               tags$br(),
               
               

               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               # actionButton("mark_line_to_rotate", "Mark lines", width = "100%"),
               # tags$br(),
               # tags$br(),
               # actionButton("rotate_average", "Rotate the whole plot", width = "100%"),
               # tags$br(),
               # tags$br(),
               # actionButton("rotate_part", "Rotate the chosen part of the plot", width = "100%"),
               # tags$br(),
               # tags$br(),
               # actionButton("rotate_down", "Shift rotated part downwards", width = "100%"),
               # tags$br(),
               # tags$br(),
               # 
               

               
               
               
               
  ), # 3 level - main layout with sidebar, sidebarPanel - Rotating plot / box 2 - Visualization
  
  
  mainPanel(
    plotlyOutput("plot_average_out"),
    plotlyOutput("plot_single_out"),
    plotlyOutput("plot_single_out2"),
    DT::dataTableOutput('data_to_rotate_out2'),
    plotlyOutput("plot_average_out2"),

    
  ), # 3 level - mainPanel - Rotating plot/ box 2 - Visualization
  
  position = 'left'), # 2 level - sidebarLayout - Rotating plot / box 2 - Visualization
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
), # /level 1 - tabPanel Rotating plot



# Final part (navbarPage) -------------------------------------------------

#switchInput color while on
tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-statusON,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-statusON {
                                        background: black;
                                        color: white;
                                        }'))),

#switchInput color while off
tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-statusOFF,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-statusOFF {
                                        background: red;
                                        color: black;
                                        }'))),



tags$head(tags$style(HTML('.bootstrap-switch.bootstrap-switch-focused {
                                  -webkit-box-shadow: none;
                                  border-color: black;
                                  box-shadow: none;
                                  outline: none;
                                  }'))),





 ) # 0 level - navbarPage












