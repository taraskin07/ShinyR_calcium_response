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



# Preliminary analysis / box 2 --------------------------------------------

  # Sidebar layout with input and output definitions /level 2 /box 2----
  tabsetPanel(
    
    # Sidebar panel for inputs: file input and preferences
    sidebarPanel(
                    actionButton("basicStat", "Calculate basic Stat for TS"),
                    tags$hr(),
      
                ) # /level 3, /box 2, sidebarPanel for statistics
    
              ), # /level 2, /box 2, tabsetPanel for statistics

  
  mainPanel(
  # Tabs
  tabsetPanel(type = "tabs",
              tabPanel("340", DT::dataTableOutput("df_340_basic_stat_out")),

              tabPanel("380", DT::dataTableOutput("df_380_basic_stat_out")),


              tabPanel("Ratio", DT::dataTableOutput("df_ratio_basic_stat_out")),


              tabPanel("Custom Ratio", DT::dataTableOutput("df_custom_ratio_basic_stat_out")),
  )
), # /level 2, /box 2, mainPanel Statistics
  


  ) # 1 level - tabPanel Data Analysis
) # 0 level - navbarPage







