fileInput("fluorescence", "Choose the excel File",
          multiple = FALSE,
          accept = c(".xls",
                     ".xlsx"))

module_UI_input_excel_data <- function(id, label = "Choose the excel File") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    # Input: Select a file ----
    fileInput(ns("fluorescence"), label = "Choose the excel File",
              multiple = FALSE,
              accept = c(".xls",
                         ".xlsx")),
    
    # Horizontal line ----
    tags$hr(),
    
    
    # Input: Checkbox if file has header ----
    checkboxInput(ns("340"), "sheet 340", TRUE),
    checkboxInput(ns("380"), "sheet 380", TRUE),
    checkboxInput(ns("Ratio"), "sheet Ratio", TRUE),
    
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons(ns("disp"), "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head")
  )
}


module_UI_input_excel_data(id='fluorescence_data')



# Module UI function
module_UI_myModule <- function(id, label = "my_label") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading")
  )
}


# Module server function
module_Server_myModule <- function(id, any_variable) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$heading,
                 quote = input$quote,
                 stringsAsFactors = stringsAsFactors)
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )    
}












library(readxl)


rename_columns <- function(df) {
  
  df_output <- df %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0("cell-", str_remove(stringr::str_split_i(.x, " ", 1), "#") 
                )
    )
  return(df_output)
}

library(writexl)

nl <- list('340'=df1, '380'=df2)

write_xlsx(
  nl,
  path = 'TempExcel.xlsx',
  col_names = TRUE,
  format_headers = TRUE,
)

time_col_name <- function(datafr) {
  if (colnames(datafr)[1] == "Time [s]") {
    colnames(datafr)[1] =  "Time"
  }
  datafr$Time <- round(datafr$Time)
  return(datafr)
}


path <-"files/2022-06-09-mpkCCD001-RawData(SOCE).xlsx"
df_output1 <- read_excel(path, sheet='340')
df_output2 <- read_excel(path, sheet='380')


df1 <- rename_columns(df_output1, 'cell-'); df1
df2 <- rename_columns(df_output1, 'cell-'); df2
df1 <- time_col_name(df1)
df2 <- time_col_name(df2)
nam <- colnames(df1[1])
nam

library(pastecs)
basic_statistics <- function(df) {
  df <- df %>% 
    distinct(across(-1))

  res <- stat.desc(df)
  res_t <- as.data.frame(t(as.matrix(res)))
  options(scipen=10)
  options(digits=3)
  return(res_t)
  
}


stat_340 <- basic_statistics(df1)
library(ggplot2)
library(plotly)
# p <- ggplot(df1, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+
#   geom_point(size = 0.2) + theme(legend.position = "none")
# 
# ggplotly(p)

df <- df1[, 1:4]; df

df_tidy_340 <- df1 %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "r.u.")

df_tidy_340

p <- ggplot(df_tidy_340, aes(Time, r.u., group = cells, color = cells)) + geom_line(linewidth=0.5)+ geom_point(size = 0.2) + theme(legend.position = "none")
ggplotly(p)






library(readxl)
dataframe_to_check <- read_excel("test/CleanTable.xlsx", sheet = "340")

clnms <-colnames(dataframe_to_check)
print(clnms)

clist <- list("cell-1", "cell-20", "cell-3")
print(clist)

what <- subset(dataframe_to_check, select = !(colnames(dataframe_to_check) %in% clist))

print(what)



typeof(what)

mb <- basic_statistics(what)


# mb$cell_names <- rownames(mb) 
# mb <- mb[, c(ncol(mb), 1:(ncol(mb)-1))] # Make row_names first column
mb <- cell_number_row(mb)
write_xlsx(list('340'=mb, '380' = mb), path = 'new_file.xlsx')




typeof(mb)




library(xts)



# Amplitude ---------------------------------------------------------------








gr <- df %>%
  group_by(cell) %>% 
  filter(df, Time < 200) %>% 
  (~ sub_range) %>% 
  # summarise(
  #   Max = max(value, na.rm = T),
  #   TimeMax = paste(Time[which(value == Max)], collapse = ", "),
  #   Min = min(sub_range$value, na.rm = T),
  #   TimeMin = paste(Time[which(value == Min)], collapse = ", "),
  #   # Amplitude = diff(pull(Max), pull(Min))
  #   Filter = filter(df, Time < 200),
  # 
  #   
  #   ) %>%
  # arrange(cell)
  # print(sub_range)

# library('gtools')

library(readxl)
cl_df <- read_excel("test/CleanTable.xlsx", sheet = "ratio")


df <- cl_df %>% 
  pivot_longer(!Time, names_to = "cell", values_to = "value")

View(df)

subset_timerange <- subset(df, (Time < 200 & Time > 5))

View(subset_timerange)

cell_index <- colnames(cl_df[-1])
cell_index

timerange_grouped <- group_by(subset_timerange, cell)

result_max <- summarize(group_by(df, cell),
  Max = max(value, na.rm = T),
  TimeMax = paste(Time[which(value == Max)], collapse = ", "))

result_min <- summarize(group_by(subset_timerange, cell),
                        Min = min(value, na.rm = T),
                        TimeMin = paste(Time[which(value == Min)], collapse = ", "))


result_amplitude <- merge(result_max, result_min, by = "cell")
result_amplitude_final <- result_amplitude %>% 
  add_column(Amplitude = result_amplitude$Max - result_amplitude$Min) %>% 
  arrange(factor(cell, cell_index))
  


gr <- df %>%
  group_by(cell) %>% 
  summarize(
    Max = max(value, na.rm = T),
    TimeMax = paste(Time[which(value == Max)], collapse = ", "),
    
    ) %>% 

    arrange(factor(cell, cell_index))

View(gr)






# Testing find_amplitude --------------------------------------------------

clean_df <- read_excel("test/CleanTable.xlsx", sheet = "ratio")
clean_df <- read_excel("test/CleanTable.xlsx", sheet = "340")

amplitude <- find_amplitude(clean_df, 23, 202)

colnames(amplitude)





