greetings <- function(default_path='files/2022-02-25-mpkCCD002-Raw(UTP).xlsx') {
  path <- readline(prompt="Enter file path/name to read table from: ")
  if (path == '') {path <- default_path}
  cat('Current path: ', path)
  return(toString(path))
}


reading_xls <- function(){
  
  path <- greetings()

# List of dataframes
  df_340 <- read_excel(path, sheet='340')
  df_380 <- read_excel(path, sheet='380')
  df_ratio <- read_excel(path, sheet='Ratio')
  df_list <- list(df_340, df_380, df_ratio)
  return(df_list)
}


# Function to fix the 'Time' column and rounding it's values
time_col_name <- function(datafr) {
  if (colnames(datafr)[1] == "Time [s]") {
    colnames(datafr)[1] =  "Time"
  }
  datafr$Time <- round(datafr$Time)
  return(datafr)
}


# Changing names of the column

rename_columns <- function(df_list, element) {

  df_output <- df_list[[element]] %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0(
                  "cell-",  
                  stringr::str_sub(.x, 2, 3) # first 2 digits after #
                        )
               )
  return(df_output)
}