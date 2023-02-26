library(shiny)
library(readxl)
library(tidyverse)
library(writexl)
library(pastecs)
library(DT)


# Reading XLS -------------------------------------------------------------
reading_xls <- function(file, disp_opt, correct_time, change_names, cnames, sheet_n){
  
  # Function just read the excel file (specific sheet == sheet_n)
  
  
  # when reading semicolon separated files,
  # having a comma separator causes `read.csv` to error
  tryCatch(
    {
      df <- read_excel(file$datapath,
                       sheet=sheet_n)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  if((correct_time%%2) == 1) {
    df <- time_col_name(df)
  }

  if((change_names%%2) == 1) {
    df <- rename_columns(df, cnames)
  }
  
  
  if(disp_opt == "head") {
    return(head(df))
  }
  else {
    return(df)
  }
  
}



# Correcting Time columns -------------------------------------------------
time_col_name <- function(datafr) {
  if (colnames(datafr)[1] == "Time [s]") {
    colnames(datafr)[1] =  "Time"
  }
  datafr$Time <- round(datafr$Time)
  return(datafr)
}



# Correcting names columns ------------------------------------------------
rename_columns <- function(df, cnames) {
  
  df_output <- df %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0(cnames, str_remove(stringr::str_split_i(.x, " ", 1), "#") 
                )
    )
  return(df_output)
}



# Dividing values of 340 & 380 dataframes -> custom_ratio -----------------
custom_ratio <- function(df1, df2) {
  df_custom_ratio <- df1/df2
  df_custom_ratio[1] <- df1[1]
  return(df_custom_ratio)
}



# Calculating basic statistics for initial time series --------------------
basic_statistics <- function(df) {
  df <- df %>% 
    distinct(across(-1))
  
  res <- stat.desc(df)
  res_t <- as.data.frame(t(as.matrix(res)))

  return(res_t)
  
}
