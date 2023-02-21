library(shiny)
source("read_xls_table.R")




# Define UI for data upload app ----
ui <- navbarPage("Calcium response plots adjustment", fluid = TRUE, position = "static-top", 

tabPanel("Data Analysis",
         
         

# App title ----
titlePanel("Choose the excel file"),


  
  # Sidebar layout with input and output definitions ----
  tabsetPanel(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("fluorescence", "Choose excel File",
                multiple = FALSE,
                accept = c(".xls",
                           ".xlsx")),
      
      # Horizontal line ----
      tags$hr(),
      

      # Input: Checkbox if file has header ----
      checkboxInput("340", "sheet 340", TRUE),
      checkboxInput("380", "sheet 380", TRUE),
      checkboxInput("Ratio", "sheet Ratio", TRUE),
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
              ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Tabs
      tabsetPanel(type = "tabs",
                  tabPanel("340", dataTableOutput("df_340")),

                  tabPanel("380", dataTableOutput("df_380")),

 
                  tabPanel("Ratio", dataTableOutput("df_ratio")),


                  tabPanel("Custom Ratio", dataTableOutput("df_custom_ratio")),
                  ),
              )
)
)







