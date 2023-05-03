source('engine.R')


# Define UI for data upload app ----
ui <- navbarPage("Calcium response plots adjustment", fluid = TRUE, position = "static-top", # 0 level

tabPanel("Preliminary analysis", # /level 1 - tabPanel Data Analysis
         
  # App title ----
  titlePanel("Upload and save"), # 2 level - titlePanel "Name of the tab"

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
      
      # Horizontal line ----
      tags$hr(),
      

      # Input: Checkbox if file has header ----
      checkboxInput("c340", "sheet 340", TRUE),
      checkboxInput("c380", "sheet 380", TRUE),
      checkboxInput("cRatio", "sheet Ratio", TRUE),
      
      # Horizontal line ----
      tags$hr(),
      
      actionButton("correct_time", "Correct 'Time' column"),
      tags$hr(),
      actionButton("change_names", "Change columns names"),
      # Horizontal line ----
      tags$hr(),
      textInput('cellName', label = 'Enter new column names', value = "cell-"),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Save as excel file
      downloadButton("SaveXlsBox1", "Save as excel file")
    
      
              )
    ), # 2 level - main layout with sidebar and tabset inside, tabsetPanel / box 1
    
  # Main panel for displaying outputs from tabsetPanel /level 2 /box 1
  mainPanel(
      # Tabs
      tabsetPanel(type = "tabs",
                  
                  tabPanel("340", DT::dataTableOutput("df_340")),

                  tabPanel("380", DT::dataTableOutput("df_380")),

 
                  tabPanel("Ratio", DT::dataTableOutput("df_ratio")),


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
              tabPanel("340", DT::dataTableOutput("df_340_basic_stat_out")),

              tabPanel("380", DT::dataTableOutput("df_380_basic_stat_out")),


              tabPanel("Ratio", DT::dataTableOutput("df_ratio_basic_stat_out")),


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
      
      tags$br('Cells to be excluded:'),
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
                
                tabPanel("340", plotlyOutput("plot340") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel 340

                tabPanel("380", plotlyOutput("plot380") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel 380
                
                tabPanel("Ratio", plotlyOutput("plot_ratio") # /level 5, /box 3, plotlyOutput
                        ), # /level 4, /box 3, tabPanel Ratio
                
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




  ) # 1 level - tabPanel Preliminary analysis
) # 0 level - navbarPage







