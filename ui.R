source('engine.R')


# Define UI for data upload app ----
ui <- navbarPage("Calcium response plots adjustment", fluid = TRUE, position = "static-top", # 0 level

tabPanel("Preliminary analysis", # 1 level - tabPanel Data Analysis
         
  # App title ----
  titlePanel("Upload and save"), # 2 level - titlePanel "Name of the tab"

  # Sidebar layout with input and output definitions ----
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
      
      # Save as excel
      downloadButton("SaveXlsBox1", "Save as excel file")
    
      
              )
    ), # 2 level - main layout with sidebar and tabset inside / box 1
    
  # Main panel for displaying outputs from tabsetPanel level 2 / box 1
  mainPanel(
      # Tabs
      tabsetPanel(type = "tabs",
                  tabPanel("340", dataTableOutput("df_340")),

                  tabPanel("380", dataTableOutput("df_380")),

 
                  tabPanel("Ratio", dataTableOutput("df_ratio")),


                  tabPanel("Custom Ratio", dataTableOutput("df_custom_ratio")),
                  )
      )# 2 level - tabsetPanel 


  
  
  
  
  ) # 1 level - tabPanel Data Analysis
) # 0 level - navbarPage







