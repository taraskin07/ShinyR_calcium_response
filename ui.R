source('engine.R')


# Define UI for data upload app ----
ui <- navbarPage(
  "Calcium response plots adjustment",
  theme = shinytheme("cosmo"),
  fluid = TRUE,
  position = "static-top",
  shinyjs::useShinyjs(),
  
  

  # Preliminary analysis -----------------------------------------------------
  
  tabPanel(
    "Preliminary analysis",
    
    
    # Panel title ----
    titlePanel("Upload and save"),
    
    
    # Preliminary analysis / box 1 --------------------------------------------
    
    
    # Sidebar layout with input and output definitions
    tabsetPanel(
      # Sidebar panel for inputs: file input and preferences
      sidebarPanel(
        # Input: Select a file
        fileInput(
          "dataTS",
          "Choose excel file",
          multiple = FALSE,
          accept = c(".xls",
                     ".xlsx")
        ),
        
        # Horizontal line
        tags$hr(),
        
        checkboxInput("cRatio", "sheet Ratio", TRUE),
        selectInput(
          'sheetRatio',
          'Select sheet for Ratio',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        checkboxInput("cNum", "sheet Numerator", FALSE),
        selectInput(
          'sheetNum',
          'Select sheet for Numerator',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        checkboxInput("cDen", "sheet Denominator", FALSE),
        selectInput(
          'sheetDen',
          'Select sheet for Denominator',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        # Horizontal line
        tags$hr(),
        
        
        numericInput(
          'step',
          'Enter step value for the Time column',
          value = 5,
          min = 0
        ),
        actionButton("correct_time", "Correct 'Time' column"),
        tags$br(),
        tags$hr(),
        radioButtons(
          "change_names",
          "Change columns names",
          choices = c(
            No_changes = "no_changes",
            Number_only = "number",
            Zeroes_in_front = "zeroes"
          ),
          selected = "no_changes"
        ),
        
        # Horizontal line
        tags$hr(),
        
        textInput('cellName', label = 'Enter new column names', value = "cell-"),
        actionButton("change_names_button", "Change columns names"),
        
        tags$hr(),
        
        
        # Input: Select number of rows to display
        radioButtons(
          "disp",
          "Display",
          choices = c(Head = "head",
                      All = "all"),
          selected = "all"
        ),
        
        # Save as excel file
        downloadButton("SaveXlsBox1", "Save as excel file")
        
        
      )
    ),
    
    # Main panel for displaying outputs from tabsetPanel /level 2 /box 1
    mainPanel(
      # Tabs
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Ratio", DT::dataTableOutput("df_ratio")),
        
        tabPanel("Numerator", DT::dataTableOutput("df_Num")),
        
        tabPanel("Denominator", DT::dataTableOutput("df_Den")),
        
        tabPanel("Num/Den", DT::dataTableOutput("df_custom_ratio")),
      )
    ),
    
    
    
    # Preliminary analysis / box 2 - STATISTICS--------------------------------------------
    
    # Sidebar layout with input and output definitions /level 2 /box 2----
    tabsetPanel(
      # Sidebar panel for inputs: file input and preferences
      sidebarPanel(
        actionButton("basicStat", "Calculate basic Stat for TS", style = "color: white; background-color: blue; border-color: black"),
        tags$hr(),
        
        # Save BASIC STATISTICS as excel file
        tags$br('Save basic statistics as excel file'),
        downloadButton("SaveXlsBoxStat", "Save as excel file"),
        
        
      )
      
    ),
    
    # Main panel for displaying outputs from tabsetPanel /level 2 /box 2
    mainPanel(
      # Tabs
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Ratio", DT::dataTableOutput("df_ratio_basic_stat_out")),
        
        tabPanel("Numerator", DT::dataTableOutput("df_Num_basic_stat_out")),
        
        tabPanel("Den", DT::dataTableOutput("df_Den_basic_stat_out")),
        
        
        tabPanel(
          "Num/Den",
          DT::dataTableOutput("df_custom_ratio_basic_stat_out")
        ),
      )
    ),
    
    
    
    # Preliminary analysis / box 3 - plots ------------------------------------
    
    # Sidebar layout with input and output definitions /level 2 /box 2----
    tabsetPanel(
      # Sidebar panel for inputs: file input and preferences
      sidebarPanel(
        selectizeInput(
          'legend_order',
          "Choose how to order the plot's legend:",
          choices = c('Native', 'Regex', 'Mixed', 'Mixed_revered'),
          selected = 'Native'
        ),
        actionButton("plot_all", "Plot all graphs", width = "49%"),
        actionButton("plot_single", "Plot single graph", width = "49%"),
        
        tags$hr(),
        
        uiOutput("tabUI"),
        
        actionButton("exclude_cell", "Exclude cell", width = "24%"),
        actionButton("exclude_undo", "Undo", width = "24%"),
        actionButton("include_cell", "Include cell", width = "24%"),
        actionButton("exclude_reset", "Reset", width = "24%"),
        
        tags$hr('Cells to be excluded:'),
        verbatimTextOutput("list_of_cells", placeholder = TRUE),
        
        tags$br(),
        actionButton(
          "new_dataframes",
          "Obtain new tables",
          style = "color: white;
                   background-color: blue;
                   border-color: black",
          width = "49%"
        ),
        
        
        actionButton("plot_new_all", "Plot all new graphs", width = "49%"),
        
        
        # Save DATA WITHOUT BAD CELLS as excel file
        tags$hr(),
        tags$br('Save new data without bad cells as excel file'),
        downloadButton("SaveXlsBoxNoBadCells", "Save as excel file"),
        
      )
    ),
    
    # Main panel for displaying outputs from tabsetPanel /level 2 /box 3
    mainPanel(
      # Tabs
      tabsetPanel(
        type = "tabs",
        id = "tab",
        
        tabPanel("Ratio", value = 'R', plotlyOutput("plot_ratio")),
        
        
        tabPanel("Num", value = 'N', plotlyOutput("plotNum")),
        
        tabPanel("Den", value = 'D', plotlyOutput("plotDen")),
        
        
        tabPanel("Num/Den", value = 'ND', plotlyOutput("plot_custom_ratio")),
        
      )
    ),
    
    
  ),
  # 1 level - tabPanel Preliminary analysis
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Analyzing amplitude -----------------------------------------------------
  
  tabPanel(
    "Analyzing amplitude",
    # /level 1 - tabPanel Analyzing amplitude
    
    # Analyzing amplitude sidebar title
    titlePanel("Upload clean excel file"),
    
    
    
    # Analyzing amplitude / box 1 --------------------------------------------
    
    # Sidebar layout with input and output definitions /level 2 /box 1
    tabsetPanel(sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        fileInput(
          "clean_file",
          "Choose excel File with 'clean' data",
          multiple = FALSE,
          accept = c(".xls",
                     ".xlsx")
        ),
        
        # Horizontal line
        tags$hr(),
        
        checkboxInput("clRatio", "sheet Ratio", TRUE),
        selectInput(
          'sheetClRatio',
          'Select sheet for Ratio',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        checkboxInput("clNum", "sheet Numerator", FALSE),
        selectInput(
          'sheetClNum',
          'Select sheet for Numerator',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        checkboxInput("clDen", "sheet Denominator", FALSE),
        selectInput(
          'sheetClDen',
          'Select sheet for Denominator',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        
        
        
      ),
      
      
      
      
      # Main panel for displaying outputs from tabsetPanel /level 2 /box 1
      mainPanel(
        # Tabs
        tabsetPanel(
          type = "tabs",
          
          tabPanel("Ratio", DT::dataTableOutput("cl_ratio")),
          
          tabPanel("Num", DT::dataTableOutput("cl_Num")),
          
          tabPanel("Den", DT::dataTableOutput("cl_Den")),
          
          tabPanel("Num/Den", DT::dataTableOutput("cl_custom_ratio")),
        )
      ),
      
    )),
    # tabsetPanel
    
    
    
    
    # Analyzing amplitude / box 2 - Plot preview ----------------
    
    tabsetPanel(sidebarLayout(
      sidebarPanel(
        selectizeInput(
          'legend_order2',
          "Choose how to order the plot's legend:",
          choices = c('Native', 'Regex', 'Mixed', 'Mixed_revered'),
          selected = 'Native'
        ),
        selectInput(
          "peaks_amount",
          'Amount of ranges to plot',
          c(Choose = '', 0, 1, 2, 3, 4),
          selectize = TRUE
        ),
        uiOutput("slider"),
        actionButton("plot_all2", "Plot all graphs", width = "49%"),
        actionButton("plot_single2", "Plot single graph", width = "49%"),
        uiOutput("tabUI2"),
        tags$hr(),
      ),
      
      
      # Main panel for displaying outputs from tabsetPanel /level 2 /box 3
      mainPanel(
        # Tabs
        tabsetPanel(
          type = "tabs",
          id = "tab2",
          
          tabPanel("Ratio", value = 'R', plotlyOutput("plot_ratio2")),
          
          
          tabPanel("Num", value = 'N', plotlyOutput("plotNum2")),
          
          tabPanel("Den", value = 'D', plotlyOutput("plotDen2")),
          
          
          tabPanel("Num/Den", value = 'ND', plotlyOutput("plot_custom_ratio2")),
        )
      ),
    )),
    
    
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
      )
      
    ),
    
    
    # Main panel for displaying outputs from tabsetPanel Analyzing amplitude /level 2 /box 2
    mainPanel(
      # Tabs
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Ratio", DT::dataTableOutput("df_ratio_amplitude_out")),
        
        tabPanel("Num", DT::dataTableOutput("df_Num_amplitude_out")),
        
        tabPanel("Den", DT::dataTableOutput("df_Den_amplitude_out")),
        
        tabPanel(
          "Num/Den",
          DT::dataTableOutput("df_custom_ratio_amplitude_out")
        ),
      )
    ),
  ),
  
  
  
  
  
  
  
  # TAB PANEL Shifting curves ---------------------------------------------------------
  
  tabPanel(
    "Shifting curves",
    
    # Panel title ----
    titlePanel("Shifting ratio plot"),
    
    
    # Shifting curves / box 1 -------------------------------------------------
    
    # Sidebar panel for inputs: file input and preferences
    
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file
        fileInput(
          "read_sheets",
          "Choose excel File with Ratio sheet",
          multiple = FALSE,
          accept = c(".xls",
                     ".xlsx")
        ),
        tags$hr(),
        selectInput(
          'sheets',
          'Select the sheet',
          '',
          selected = '',
          multiple = FALSE
        ),
      ),
      
      
      mainPanel(DT::dataTableOutput("dt_to_shift_out")),
      
      position = 'left'
    ),
    
    
    
    # Shifting curves / box 2 -------------------------------------------------
    
    
    # Sidebar panel for plots
    
    sidebarLayout(
      sidebarPanel(
        style = "height: 100%",
        
        tags$br("Enter timeframe for the baseline and region to analyze"),
        tags$hr(),
        numericInput("start_t_shift", "Region to analyze START: (sec)", 0),
        numericInput("end_t_shift", "Region to analyze END: (sec)", 500),
        tags$hr(),
        selectInput(
          "cellShiftInput",
          'Choose a trace',
          choices = '',
          selected = '',
          selectize = FALSE,
          multiple = FALSE
        ),
        
        actionButton("plots_init_single", "Render single plot", width = "49%"),
        actionButton("plots_init_all", "Render all plots", width = "49%"),
        tags$hr(),
        tags$div(
          div(
            style = "display: inline-block; width: 59%;",
            numericInput("max_lag", "Enter maximum lag for CCF (steps)", 10)
          ),
          div(
            style = "display: inline-block; width: 40%;",
            actionButton("shift_curves", "Shift the curves using CCF", width = "100%")
          )
        ),
        tags$head(tags$style(
          HTML(".red-text { color: red; font-size: 24px;}")
        )),
        tags$strong(class = "red-text", "!!! OR !!!"),
        tags$br(style = "line-height: 0.1;"),
        tags$div(
          div(
            style = "display: inline-block; width: 59%;",
            numericInput("response_window", "Enter response time: (sec)", 100)
          ),
          div(
            style = "display: inline-block; width: 40%;",
            actionButton("shift_maximum", "Shift Maximum", width = "100%")
          )
          
        ),
        
        tags$hr(),
        actionButton("shift_reset", "Reset all shifting"),
        tags$hr(style = 'border-style: inset;'),
        actionButton("plots_shift_single", "Render single shifted plot", width = "49%"),
        actionButton("plots_shift_all", "Render all shifted plots", width = "49%"),
        
        # Save SHIFTED curves as excel file
        tags$br(),
        tags$br('Save shifted curves as excel file'),
        
        tags$div(
          style = "display: flex; align-items: center;text-align: center; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; height: 100%;",
          downloadButton("SavePltsShift", "Save as excel file", style = "height: 100%;"),
          div(
            style = "margin-top: 15px;margin-left: 30px;",
            switchInput(
              inputId = "plots_shift_omit",
              label = "Omit NA values in columns",
              value = FALSE,
              size = "normal",
              onStatus = "statusON",
              offStatus = "statusOFF",
              onLabel = "omitted",
              offLabel = "with NA",
              labelWidth = "auto",
              width = "100%"
            )
          )
        )
        
        
      ),
      
      
      mainPanel(
        h3(class = "center-h3", "Before lag correction"),
        plotlyOutput("plot_shift_upper"),
        h3(class = "center-h3", "After lag correction"),
        plotlyOutput("plot_shift_lower"),
        DT::dataTableOutput("lag_values_df_out"),
        
        
      ),
      
      position = 'left'
    ),
    
    
    
    # Sidebar panel for plots with average
    
    
    sidebarLayout(
      sidebarPanel(
        style = "max-height: 100%",
        
        tags$br("Calculate average for shifted and initial data"),
        tags$hr(),
        actionButton("plots_average_init", "Render initial average", width = "49%"),
        actionButton("plots_average_shifted", "Render shifted average", width = "49%"),
        tags$hr(),
        
        # Save SHIFTED curves as excel file
        tags$br('Save shifted and initial curves as excel file'),
        downloadButton("SaveAverage", "Save as excel file"),
        
        
        
        
      ),
      
      
      mainPanel(
        h3(class = "center-h3", "Before lag correction"),
        plotlyOutput("plot_average_upper"),
        h3(class = "center-h3", "After lag correction"),
        plotlyOutput("plot_average_lower"),
      ),
      
      position = 'left'
    ),
    
    
  ),
  
  
  
  
  
  # TAB PANEL Rotating plot -------------------------------------------------
  
  
  
  tabPanel(
    "Rotating plot",
    # /level 1 - tabPanel Rotating plot
    
    
    
    # Panel title ----
    titlePanel("Upload data to rotate"),
    # 2 level - titlePanel "Shifting ratio plot"
    
    
    
    # Rotating plot / box 1 -------------------------------------------------
    
    
    # Sidebar panel for inputs: file input and preferences
    
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        fileInput(
          "read_curves",
          "Choose excel File",
          multiple = FALSE,
          accept = c(".xls",
                     ".xlsx")
        ),
        
        # Horizontal line
        tags$hr(),
        
        selectInput(
          "data_sheets",
          'Select the sheet in file',
          '',
          selected = '',
          multiple = FALSE
        ),
        
        
        
        
        
      ),
      # 3 level - main layout with sidebar, sidebarPanel - Rotating plot / box 1
      
      
      mainPanel(DT::dataTableOutput("data_to_rotate_out")),
      # 3 level - mainPanel - Rotating plot / box 1
      
      position = 'left'
    ),
    # 2 level - sidebarLayout - Rotating plot / box 1

    
    
    
    # Rotating plot / box 2 - Visualization ------------------------------------
    
    
    
    
    # Sidebar panel for plots
    
    sidebarLayout(
      sidebarPanel(
        style = "height: 100%",
        
        tags$br("Enter timeframe for the line to rotate and region to calculate area"),
        tags$hr(),
        actionButton("render_plot_with_average", "Render average plot", width = "100%"),
        
        
        
        
        tags$br(),
        tags$br(),
        
        tags$div(
          style = 'display: flex;
                       justify-content: space-between;
                       text-align: center;
                       white-space: nowrap;
                       overflow: hidden;
                       text-overflow: ellipsis;',
          
          switchInput(
            inputId = "rotate_average",
            label = "Rotate the whole plot",
            value = FALSE,
            size = "normal",
            onStatus = "statusON",
            offStatus = "statusOFF",
            onLabel = "Rotate",
            offLabel = "OFF",
            labelWidth = "auto",
            width = "49%"
          ),
          switchInput(
            inputId = "mark_line_to_rotate",
            label = "Mark lines",
            value = TRUE,
            size = "normal",
            onStatus = "statusON",
            offStatus = "statusOFF",
            labelWidth = "auto",
            width = "49%"
          )
        ),
        
        tags$div(
          style = 'display: flex;
                       justify-content: space-between;
                       text-align: center;
                 white-space: nowrap;
                 overflow: hidden;
                 text-overflow: ellipsis;',
          switchInput(
            inputId = "rotate_part",
            label = "Rotate the chosen part of the plot",
            value = FALSE,
            size = "normal",
            onStatus = "statusON",
            offStatus = "statusOFF",
            onLabel = "Rotate",
            offLabel = "OFF",
            labelWidth = "auto",
            width = "49%"
          ),
          switchInput(
            inputId = "rotate_down",
            label = "Shift rotated part downwards",
            value = TRUE,
            size = "normal",
            onStatus = "statusON",
            offStatus = "statusOFF",
            onLabel = "Rotate",
            offLabel = "OFF",
            labelWidth = "auto",
            width = "49%"
          )
        ),
        
        actionButton("reset_plot", "Reset plot to initial", width = "100%"),
        
        
        
        tags$hr(),
        numericInput("line_start", "Line to rotate START: (sec)", 0),
        numericInput("line_end", "Line to rotate END: (sec)", 120),
        tags$br(),
        numericInput("flat_start", "Plot to rotate START: (sec)", 200),
        numericInput("flat_end", "Plot to rotate END: (sec)", 500),
        
        
        
        
        # Plotting single graph and rotate
        tags$hr(),
        numericInput("cell_to_plot_to_rotate", "Enter number of cell", 1),
        actionButton(
          "plot_single_to_rotate",
          "Plot single graph (Unsaved manipulations are skipped)",
          width = "100%"
        ),
        
        tags$br(),
        tags$br(),
        
        actionButton("rotate_single_plot", "Rotate single plot"),
        actionButton("rotate_single_plot_part", "Rotate the part of the single plot"),
        switchInput(
          inputId = "rotate_single_down",
          label = "Shift down",
          value = TRUE,
          size = "normal",
          onStatus = "statusON",
          offStatus = "statusOFF",
          onLabel = "Shift",
          offLabel = "OFF"
        ),
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
        actionButton(
          "rotate_all_cells_from_scratch",
          "Rotate all cells from scratch",
          style = "color: white; background-color: blue; border-color: black"
        ),
        switchInput(
          inputId = "rotate_baseline_as_well",
          label = "Rotate baseline as well",
          value = TRUE,
          size = "normal",
          onStatus = "statusON",
          offStatus = "statusOFF",
          onLabel = "Rotate",
          offLabel = "OFF"
        ),

        
        # Save SHIFTED curves as excel file
        tags$br(),
        tags$br('Save final curve as excel file'),
        downloadButton("SaveFinal", "Save as excel file"),
        
        
        tags$hr(style = 'border-style: inset;'),
        
        
        
        actionButton("plot_rotated_result", "Plot rotated average result", width = "100%"),
        
        
        # Plotting ROTATED single graph
        tags$hr(),
        numericInput("rotated_plots", "Enter number of cell", 1),
        actionButton("render_rotated_plots", "Plot single graph", width = "100%"),
        
        

        tags$hr('Define the baseline timeline and the region with maximum'),
        tags$br(),
        numericInput("baseline_start", "Baseline START: (sec)", 0),
        numericInput("baseline_end", "Baseline END: (sec)", 120),
        tags$hr(),
        tags$hr(),
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!--------------------------------------------------------------------
        numericInput("area_start", "Calculate area START: (sec)", 125, step = 5),
        numericInput("area_end", "Calculate area END: (sec)", 300, step = 5),
        tags$hr(),
        actionButton("mark_line_to_calculate", "Mark area", width = "100%"),
        tags$br(),
        tags$br(),
        actionButton(
          "calculate_area",
          "Calculate area for all curves",
          style = "color: white;
                            background-color: blue;
                            border-color: black"
        ),
        tags$br(),
        tags$br(),
        actionButton(
          "save_area_single",
          "Save manually calculated area for the current curve"
        ),
        tags$br(),
        tags$br(),
        actionButton(
          "calculate_area_statistics",
          "Calculate statistics",
          style = "color: white;
               background-color: blue;
               border-color: black"
        ),
        
        tags$br('The calculated area is:'),
        verbatimTextOutput("area_value", placeholder = TRUE),
        tags$br(),
        downloadButton("SaveAreaStatistics", "Save statistics as excel file"),

        
      ),
      
      
      mainPanel(
        plotlyOutput("plot_average_out"),
        plotlyOutput("plot_single_out"),
        plotlyOutput("plot_single_out2"),
        DT::dataTableOutput('data_to_rotate_out2'),
        plotlyOutput("plot_average_out2"),
        plotlyOutput("plot_single_area_out"),
        DT::dataTableOutput('area_data_out'),
        DT::dataTableOutput('result_statistics_out'),
        
        
      ),
      
      position = 'left'
    ),
    
  ),
  # /level 1 - tabPanel Rotating plot
  
  
  
  # Final part (navbarPage) -------------------------------------------------
  
  #switchInput color while on
  tags$head(tags$style(
    HTML(
      '.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-statusON,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-statusON {
                                        background: black;
                                        color: white;
                                        }'
    )
  )),
  
  #switchInput color while off
  tags$head(tags$style(
    HTML(
      '.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-statusOFF,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-statusOFF {
                                        background: red;
                                        color: black;
                                        }'
    )
  )),
  
  
  
  tags$head(tags$style(
    HTML(
      '.bootstrap-switch.bootstrap-switch-focused {
                                  -webkit-box-shadow: none;
                                  border-color: black;
                                  box-shadow: none;
                                  outline: none;
                                  }'
    )
  )),
  
  
  
  tags$style(HTML(".center-h3 { text-align: center; }")),
  
  
) # 0 level - navbarPage
