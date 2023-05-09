library(shiny)
library(readxl)
library(tidyverse)
library(writexl)
library(pastecs)
library(DT)
library(ggplot2)
library(plotly)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Preliminary analysis ----------------------------------------------------- 


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




# Plotting ggploly graph --------------------------------------------------

ggplotly_render <- function(df_n) {
  df <- df_n %>% 
    pivot_longer(!Time, names_to = "cells", values_to = "r.u.") 
  
  p <- ggplot(df, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+ geom_point(size = 0.2)
  return(ggplotly(p))
}


# Choose the specific column of the DataFrame -----------------------------

get_col_names <- function(df, cell_name, cell_number) {
  col <- paste0(cell_name, cell_number)
  time_col <- colnames(df[1])
  return(df[c(time_col, col)])
}


# Make "cell_number" first column of Basic Statistics dataframe

cell_number_row <- function(df) {
  df$cell_number <- rownames(df) 
  df <- df[, c(ncol(df), 1:(ncol(df)-1))]
  return(df)
}





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Analyzing amplitude ----------------------------------------------------- 




# Reading CLEAN XLS -------------------------------------------------------------
reading_clean_xls <- function(file, sheet_n){
  
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
  
} # reading_clean_xls




# Calculating amplitude ---------------------------------------------------

find_amplitude <- function(clean_df, min_time, max_time) {
  
  # Obtaining long-format dataframe
  df_long <- clean_df %>% 
    pivot_longer(!Time, names_to = "cell", values_to = "value")
  
  # For future sorting saving correct order of cell names
  cell_index <- colnames(clean_df[-1])

  # Obtaining subset for baseline in accordance with min_time and max_time
  subset_timerange <- subset(df_long, (Time > min_time & Time < max_time))
  
  # Maximum values for initial datase
  result_max <- summarize(group_by(df_long, cell),
                          Maximum = max(value, na.rm = T),
                          Max_Time = paste(Time[which(value == Maximum)], collapse = ", "),
                          Global_Min_Values = min(value, na.rm = T),
                          Global_Min_Time = paste(Time[which(value == Global_Min_Values)], collapse = ", "))
  
  
  # Minimum values for baseline subset
  result_min <- summarize(group_by(subset_timerange, cell),
                          Baseline_Minimum = min(value, na.rm = T),
                          Baseline_Min_Time = paste(Time[which(value == Baseline_Minimum)], collapse = ", "))
  
  # Inner join of two resulting tables
  result_amplitude <- merge(result_max, result_min, by = "cell")
  
  # Adding additional columns
  result_amplitude_final <- result_amplitude %>% 
    add_column(Amplitude = (result_amplitude$Maximum - result_amplitude$Baseline_Minimum)) %>% 
    arrange(factor(cell, cell_index)) %>% 
    select(cell, Global_Min_Values, Baseline_Minimum, Maximum, Amplitude, Baseline_Min_Time, Max_Time, Global_Min_Time)
  
  return(result_amplitude_final)
}
