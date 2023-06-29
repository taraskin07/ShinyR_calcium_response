library(renv)
library(shiny)
library(shinythemes)
library(readxl)
library(writexl)
library(pastecs)
library(DT)
library(ggplot2)
library(plotly)
library(gtools)
library(tidyverse)
library(DescTools)
library(randomcoloR)
library(shinyWidgets)




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

  
  if(change_names== "zeroes") {
    df <- rename_columns_zeros(df, cnames)
  } else if(change_names== "number") {
    df <- rename_columns(df, cnames)
  }
  
  
  if(disp_opt == "head") {
    return(head(df))
  }
  else {
    return(df)
  }
  
}


# Extracting name ---------------------------------------------------------

filename <- function(file_name, str_name){
  name <- unlist(strsplit(toString(file_name), split = '[.]'))
  res_name <- paste(name[1], str_name, sep='-')
  return(res_name)
}



# CORRECT LATER -----------------------------------------------------------

custom_filename <- function(file_name, str_name) {
  
  nname <- str_extract(file_name, '(^\\d{4}-\\d{2}-\\d{2}-\\w+\\d+)\\D', group = 1)
  res_name <- paste0(nname, '-', str_name, '.xlsx')
  return(res_name)
}


# Correcting Time columns -------------------------------------------------
time_col_name <- function(datafr) {

    time_col <- '([Tt]ime\\s?)'
    colnames(datafr)[grepl(time_col, colnames(datafr))] =  "Time"
  
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

adding_zeroes <- function(vctr) {
  
  if (stringr::str_length(vctr)==1) {strnum <- paste0('00', vctr)
  } else if (stringr::str_length(vctr)==2) {strnum <- paste0('0', vctr)
  } else {strnum <- toString(vctr)}
  
  return(strnum)
}

rename_columns_zeros <- function(df, cnames) {
  
  df_output <- df %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0(cnames, unname(sapply(str_remove(stringr::str_split_i(.x, " ", 1), "#"), adding_zeroes)) 
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
  res_a <- data.frame(Cell = rownames(res_t), Min = decim(res_t$min, 3), Max = decim(res_t$max, 3), Difference = decim(res_t$range, 3), Mean = decim(res_t$mean, 3), Median = decim(res_t$median, 3), SD.mean = decim(res_t$std.dev, 3), SE.mean = decim(res_t$SE.mean, 3), Time_points_amount = res_t$nbr.val, Missing = res_t$nbr.na)
  

  return(res_a)
  
}




# Plotting ggploly graph --------------------------------------------------

ggplotly_render <- function(df_n, baseline = FALSE, b_min = 0, b_max = 120, region = FALSE, r_min = 130, r_max = 330, ready = TRUE) {
  
  df_n <- time_col_name(df_n)
  
  df <- df_n %>% 
    pivot_longer(!Time, names_to = "cells", values_to = "Signal") 
  
  unique_vals <- length(unique(df$cells))
  
    p <- ggplot(df, aes(Time, Signal, group = cells, color = cells)) + 
      geom_line(size=0.5) + geom_point(size = 0.2) + 
      scale_color_manual(values=randomColor(count = unique_vals, hue = 'random', luminosity = 'bright'))
    
    if (baseline == T) {
      p <- p + 
        geom_vline(xintercept = b_max, colour="black", linetype = "longdash") +
        geom_vline(xintercept = b_min, colour="black", linetype = "longdash")
    }
    
    if (region == T) {
      p <- p + 
        geom_vline(xintercept = r_max, colour="red", linetype = "dotted") +
        geom_vline(xintercept = r_min, colour="red", linetype = "dotted") 
    }
  
    if (ready == T) {return(ggplotly(p))} else {return(p)}
  
  
}



# Constructing cell's names --------------------------------------------------

constructing_names <- function(cell_name, cell_number, format) {
  
  if(format == "zeroes") {
  
  comb <- paste0(cell_name, adding_zeroes(cell_number))
  
  } else {comb <- paste0(cell_name, cell_number)}
  
  return(comb)
  }

# CORRECT LATER -----------------------------------------------------------
# Choose the specific column of the DataFrame -----------------------------

get_col_names <- function(df, cell_name, cell_number, format) {
  
  col <- constructing_names(cell_name, cell_number, format)
  
  time_col <- colnames(df[1])
  
  return(df[c(time_col, col)])
  
}



# CORRECT LATER -----------------------------------------------------------


# Function to find a specific cell related to its number
finding_cell_name <- function(data, number){
  
  list_of_names <- colnames(data)
  match <- paste0('(\\D0*)', number, '($|\\s)')
  cell_name <- list_of_names[grepl(match, list_of_names)]
  
  if (length(cell_name)>1) {stop("More than one column with similar name!")}
  return(cell_name)}


# get_col_names alternative

single_plot <- function(df, cell_number) {
  
  df_time <- time_col_name(df)
  col <- finding_cell_name(df_time, cell_number)
  
  return(df_time[c('Time', col)])
  
}




# Make "cell_number" first column of Basic Statistics dataframe

cell_number_row <- function(df) {
  df$cell_number <- rownames(df) 
  df <- df[, c(ncol(df), 1:(ncol(df)-1))]
  return(df)
}






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

find_amplitude <- function(clean_df, min_time, max_time, start_time, end_time) {
  
  # Obtaining long-format dataframe
  df_long <- clean_df %>% 
    pivot_longer(!Time, names_to = "cell", values_to = "value")
  
  # For future sorting saving correct order of cell names
  cell_index <- colnames(clean_df[-1])

  # Obtaining subset for baseline in accordance with min_time and max_time
  subset_timerange <- subset(df_long, (Time >= min_time & Time <= max_time))
  
  # Obtaining subset for the Region of interest in accordance with start_time and end_time
  subset_regint <- subset(df_long, (Time >= start_time & Time <= end_time))
  
  # Maximum values for initial dataset
  result_max <- summarize(group_by(subset_regint, cell),
                          Maximum = decim(max(value, na.rm = T), 3),
                          Max_Time = paste(Time[which(value == Maximum)], collapse = ", "))

  
  # Global values for initial dataset
  result_global <- summarize(group_by(df_long, cell),
                         Global_Min_Values = decim(min(value, na.rm = T), 3),
                         Global_Min_Time = paste(Time[which(value == Global_Min_Values)], collapse = ", "),
                         Global_Max_Time = paste(Time[which(value == max(value, na.rm = T))], collapse = ", "))
    
  
  
  
  # Minimum values for baseline subset
  result_min <- summarize(group_by(subset_timerange, cell),
                          Baseline_Median = decim((median(value, na.rm = T)), 3), 
                          Baseline_SD = decim(sd(value, na.rm = T), 3),
                          Baseline_CV = decim(100*sd(value, na.rm = T)/mean(value, na.rm = T), 0),
                          Baseline_Minimum = decim(min(value, na.rm = T), 3),
                          Baseline_Mean = decim(mean(value, na.rm = T), 3),
                          Baseline_Min_Time = paste(Time[which(value == Baseline_Minimum)], collapse = ", "))
  
  # Inner join of two resulting tables
  result_amplitude <- merge(result_max, result_min, by = "cell")
  result_amplitude <- merge(result_amplitude, result_global, by = "cell")
  
  # Adding additional columns - Difference and Amplitude
  result_amplitude_final <- result_amplitude %>% 
    add_column(
      Difference = abs(decim((result_amplitude$Baseline_Median - result_amplitude$Baseline_Mean), 3))
      ) %>% 
    add_column(
      Amplitude = decim((result_amplitude$Maximum - as.numeric(result_amplitude$Baseline_Median)), 3)
      ) %>% 
    arrange(factor(cell, cell_index)) %>% 
    select(cell, Amplitude, Maximum, Baseline_Median, Baseline_Mean, Difference, Baseline_SD, Baseline_CV, Global_Min_Values, Global_Min_Time, Baseline_Min_Time, Max_Time, Global_Max_Time)
  
  return(result_amplitude_final)

}

   

# Another function to calculate MINIMUNM (380 nm) - refactor later!!!!



find_amplitude_380 <- function(clean_df, min_time, max_time, start_time, end_time) {
  
  # Obtaining long-format dataframe
  df_long <- clean_df %>% 
    pivot_longer(!Time, names_to = "cell", values_to = "value")
  
  # For future sorting saving correct order of cell names
  cell_index <- colnames(clean_df[-1])
  
  # Obtaining subset for baseline in accordance with min_time and max_time
  subset_timerange <- subset(df_long, (Time >= min_time & Time <= max_time))
  
  # Obtaining subset for the Region of interest in accordance with start_time and end_time
  subset_regint <- subset(df_long, (Time >= start_time & Time <= end_time))
  
  # Minimum values for initial dataset
  result_min <- summarize(group_by(subset_regint, cell),
                          Minimum = decim(min(value, na.rm = T), 3),
                          Min_Time = paste(Time[which(value == Minimum)], collapse = ", "))
  
  
  # Global values for initial dataset
  result_global <- summarize(group_by(df_long, cell),
                          Global_Max_Values = decim(max(value, na.rm = T), 3),
                          Global_Max_Time = paste(Time[which(value == Global_Max_Values)], collapse = ", "),
                          Global_Min_Time = paste(Time[which(value == min(value, na.rm = T))], collapse = ", "))
  
  
  # Maximum values for baseline subset
  result_max <- summarize(group_by(subset_timerange, cell),
                          Baseline_Median = decim((median(value, na.rm = T)), 3), 
                          Baseline_SD = decim(sd(value, na.rm = T), 3),
                          Baseline_CV = decim(100*sd(value, na.rm = T)/mean(value, na.rm = T), 0),
                          Baseline_Maximum = decim(max(value, na.rm = T), 3),
                          Baseline_Mean = decim(mean(value, na.rm = T), 3),
                          Baseline_Max_Time = paste(Time[which(value == Baseline_Maximum)], collapse = ", "))
  
  # Inner join of two resulting tables
  result_amplitude <- merge(result_min, result_max, by = "cell")
  result_amplitude <- merge(result_amplitude, result_global, by = "cell")
  
  # Adding additional columns - Difference and Amplitude
  result_amplitude_final <- result_amplitude %>% 
    add_column(
      Difference = abs(decim((result_amplitude$Baseline_Mean - result_amplitude$Baseline_Median), 3))
      ) %>% 
    add_column(
      Amplitude = -decim((result_amplitude$Minimum - as.numeric(result_amplitude$Baseline_Median)), 3)
      ) %>% 
    arrange(factor(cell, cell_index)) %>% 
    select(cell, Amplitude, Minimum, Baseline_Median, Baseline_Mean, Difference, Baseline_SD, Baseline_CV, Global_Max_Values, Global_Max_Time, Baseline_Max_Time, Min_Time, Global_Min_Time)
  
  return(result_amplitude_final)

  
  
}






# Rounding values ---------------------------------------------------------

decim <- function(number, digits) {
  
  
  round_result <- as.numeric(format(round(number, digits), nsmall = digits))
  
  return(round_result)
}





# Amplitude Statistics ----------------------------------------------------

summarize_amplitudes <- function(amplitude, excl_df) {

ampl_calculating <- amplitude %>% 
  summarize(
    Amplitude_average = decim(mean(Amplitude, na.rm = T), 3),
    Amplitude_SD = decim(sd(Amplitude, na.rm = T), 3),
    Amplitude_3SD = decim(3*Amplitude_SD, 3),
    Amplitude_CV_percent = decim(100*Amplitude_SD/Amplitude_average, 2),
    Average_Baseline_CV_percent = decim(mean(Baseline_CV, na.rm = T), 2),
    Amount_of_cells = nrow(amplitude)
  ) %>% 
  add_column(Bad_cells = nrow(excl_df)) %>% 
  add_column(Percent_of_bad_cells = 100*nrow(excl_df)/nrow(amplitude)) %>% 
  select(Amplitude_average, Amplitude_CV_percent, Average_Baseline_CV_percent, Amount_of_cells, Bad_cells, Percent_of_bad_cells, Amplitude_SD, Amplitude_3SD)



return(as.data.frame(ampl_calculating))

}




# Shifting curves ---------------------------------------------------------


# Basic function to determine lag and its sign between two series of values
shift <- function(maximum_after, maximum_before, max_lag) {
  
  # Cross-correlation function, omits any NA values and skip plot rendering
  mtrx <- ccf(maximum_after, maximum_before, lag.max = max_lag, na.action=na.omit, plot=FALSE)
  
  #To get data from resulting table - new dataframe is constructed with all the necessary information
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
  
  #Lag that is corresponded to the maximum CCF (ACF) value
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  
  # Can be >0 (maximum_after, maximum_before) or <0 (maximum_before, maximum_after) or 0 (maximum_before = maximum_after)
  return(lag_for_max_acf)
}


# Borrowed function to add columns with different length to dataframe (empty values are NA) 
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}



# Function to find curve with maximum that is closer to the left (earlier time) and shift the reference to it
finding_shifted_curve <- function(df, main_cell_number, lower, upper, max_lag) {
  
  # Fixing the 'Time' column if necessary
  df_time <- time_col_name(df)
  # print(paste0('1    ',df_time))
  # Obtaining list of dataframe column names
  list_of_names <- colnames(df_time)
  # print(paste0('2    ',list_of_names))
  # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
  subset_timerange <- as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))
  # print(paste0('3    ',subset_timerange))
  # Regular expression to identify cell with the number input given
  match <- paste0('(\\D0*)', main_cell_number, '($|\\s)')
  # print(paste0('4    ',match))
  # Current reference cell which probably would be changed (its number) during the process of finding the one with the earliest maximum
  reference <- list_of_names[grepl(match, list_of_names)]
  
  if (length(reference)>1) {stop("More than one column with similar name!")}
  # print(paste0('reference    ',reference))
  # The cell, that was chosen by the operator as a reference
  main_cell <- list_of_names[grepl(match, list_of_names)]
  # print(paste0('main_cell    ',main_cell))
  # Excluding time column from column names list
  coln_df <- list_of_names[list_of_names != "Time"]
  # print(coln_df)
  # Finding the cell with the earliest maximum, skipping the Time column (coln_df instead of list_of_names)
  for (cell in coln_df) {
    
    if (shift(subset_timerange[, cell], subset_timerange[, reference], max_lag) < 0) {
      # print(paste0('7    ',cell))
      reference <- cell
      
    }
    
  }
  
  
  # Shifting main series to the left in order to correlate with the reference (with the earliest maximum)
  # Position of the maximum CCF (ACF) value = lag to choose
  lag_for_max_acf <- shift(subset_timerange[, main_cell], subset_timerange[, reference], max_lag)
  # print(lag_for_max_acf)
  # print(length(subset_timerange[, main_cell]))
  
  # Shifting initial dataframe column, related to the main_cell, that was chosen by the operator
  shifted_main_cell_values <- as.data.frame(subset_timerange[, main_cell][(lag_for_max_acf+1):length(subset_timerange[, main_cell])])
  colnames(shifted_main_cell_values) <- main_cell

  # # Series of values, shifted to the left, without NA values (shorter series instead) from the main cell column that was chosen
  return(shifted_main_cell_values)
  
}




shifting_curves <- function(df, shifted_main_cell_values, lower, upper, max_lag, counterv = 1) {
  
  # Fixing the 'Time' column if necessary
  df_time <- time_col_name(df)
  
  # Obtaining list of dataframe column names
  list_of_names <- colnames(df_time)
  
  # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
  subset_timerange <- subset(df_time, (Time >= lower & Time <= upper))
  
  
  # Excluding time column from column names list
  coln_df <- list_of_names[list_of_names != "Time"]
  
  #Resulting dataframe
  result_df <- data.frame(Time = df_time$Time)
  # lag_data <- data.frame(A = character(), B = numeric())
  # colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])
  
  
  
  if (counterv > 20) {
    
    stop('Too many iterations! Increase the interval, maximum lag or delete invalid curves!')}
  
  for (cell in coln_df) {
    
    lag_for_max_acf <- shift(subset_timerange[, cell], shifted_main_cell_values, max_lag)
    

    
    if (lag_for_max_acf < 0) {
      
      main_cell_number <- str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
      print(cell)
      print(paste0("Main cell number now is: ", main_cell_number))
      shifted_main_cell_values <- finding_shifted_curve(df, main_cell_number, lower, upper, max_lag)
      counterv = counterv + 1
      print(paste0('The counter variable is: ', counterv))
      res <- shifting_curves(df, shifted_main_cell_values, lower, upper, max_lag, counterv)
      return(res)
      
    } else {
      
      
      shifted_cell_values <- df[, cell][(lag_for_max_acf+1):nrow(df[, cell]),]
      
      result_df <- as.data.frame(cbind.fill(result_df, shifted_cell_values))}
    
  }
  
  return(result_df)
}

# The same function but for constructing dataframe that has information about iterations and maximum_lag


shifting_curves_info <- function(lag_data, df, shifted_main_cell_values, lower, upper, max_lag, counterv = 1) {
  
  # Fixing the 'Time' column if necessary
  df_time <- time_col_name(df)
  
  # Obtaining list of dataframe column names
  list_of_names <- colnames(df_time)
  
  # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
  subset_timerange <- subset(df_time, (Time >= lower & Time <= upper))
  
  
  # Excluding time column from column names list
  coln_df <- list_of_names[list_of_names != "Time"]
  

  
  if (counterv > 20) {
    return(lag_data)
    stop('Too many iterations! Increase the interval, maximum lag or delete invalid curves!')
    }
  
  for (cell in coln_df) {
    
    lag_for_max_acf <- shift(subset_timerange[, cell], shifted_main_cell_values, max_lag)
    # print(paste0('Lag for ', cell, ': ', lag_for_max_acf))
    df_to_merge <- data.frame(A = cell, B = lag_for_max_acf)

    colnames(df_to_merge) <- c('Cell_name', colnames(shifted_main_cell_values)[1])
    # print(df_to_merge)
    
    
    if (!(colnames(shifted_main_cell_values)[1] %in% colnames(lag_data)) & (nrow(lag_data) > 0)) {

      lag_data <- as.data.frame(bind_rows(lag_data, df_to_merge))
      # print(lag_data)
    } else if (counterv == 1) {
      lag_data <- as.data.frame(rbind(lag_data, df_to_merge))
      # print(lag_data)
    } else {
      
      lag_data <- as.data.frame(bind_rows(lag_data, df_to_merge))
      print(lag_data)
      }

    

    
    
    

    
    if (lag_for_max_acf < 0) {
      
      main_cell_number <- str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
      # print(paste0("Main info number now is: ", main_cell_number))
      shifted_main_cell_values <- finding_shifted_curve(df, main_cell_number, lower, upper, max_lag)
      counterv = counterv + 1
      # print(paste0('The info variable is: ', counterv))
      res <- shifting_curves_info(lag_data, df, shifted_main_cell_values, lower, upper, max_lag, counterv)
      return(res)
      
    } else {info_df <- lag_data}
  

                        }
  
  return(info_df)
}






# Rotating plots ----------------------------------------------------------




average_curve <- function(df_read) {
  
  df_rot <- time_col_name(df_read)
  
  if (length(grep("^([Aa]verage|[Mm]ean)", colnames(df_rot))) == 0) {
    
    df_rot <- df_rot %>% 
      add_column(Average = rowMeans(df_rot[-grep('^Time$', colnames(df_rot))]))
    
    
    df_rot <- df_rot[, c(grep('^Time$', colnames(df_rot)), grep("^([Aa]verage|[Mm]ean)", colnames(df_rot)))]
    
  } else {
    
    df_rot <- df_rot %>% 
      select(c(grep('^Time$', colnames(df_rot)), grep("^([Aa]verage|[Mm]ean)", colnames(df_rot))))
  }
  
  return(df_rot)
  
}


getting_a_slice_of_df <- function(df_to_slice, cell_number) {
  
  df_to_slice <- time_col_name(df_to_slice)
  cell_name <- finding_cell_name(df_to_slice, cell_number)
  
  df_to_slice <- df_to_slice %>%
    select('Time', all_of(cell_name))
  
  return(df_to_slice)
  
}




rotating_plot <- function(df_to_rotate, lower_t, upper_t, part = FALSE, shift_down = FALSE) {
  
  
  if (ncol(df_to_rotate) != 2) {stop(print("Something wrong with the data: no such cell number or they are repeats!"))
    
  } else if (colnames(df_to_rotate)[1] != 'Time') {
    
    if (length(grep('([Tt]ime\\s?)', colnames(df_to_rotate))) != 1) {
      
      stop(print("Something wrong with the data: no Time column!"))
      
    } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 1) {
      
      colnames(df_to_rotate)[1] <- 'Time'
      
      
    } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 2) {
      
      colnames(df_to_rotate)[2] <- 'Time'
      df_to_rotate <- df_to_rotate[, c(2,1)]
      
    } else {
      
      stop(print("Something wrong with the data: no Time column!"))
      
    }
    
    
  } 
  
  
  
  initial_col_name <- colnames(df_to_rotate)[2]
  
  colnames(df_to_rotate)[2] <- 'Cell'
  
  
  
  df_1 <- subset(df_to_rotate, Time < lower_t) 
  
  df_2 <- subset(df_to_rotate, (Time >= lower_t & Time <= upper_t))
  
  df_3 <- subset(df_to_rotate, Time > upper_t) 
  
  
  # Rotating
  
  average_lm <- coef(lm(Cell ~ Time, data = df_2))
  
  # b = average_lm[[1]]
  k = average_lm[[2]]
  
  
  
  # Rotate partially or the whole plot?
  
  if (part == T) {
    
    df_2$Cell <- df_2$Cell-k*df_2$Time
    
    if (shift_down == TRUE) {
      
      df_2$Cell <- df_2$Cell + k*df_2$Time[length(df_2$Time)]
      
    }
    
    
    df_to_rotate <- rbind(df_1, df_2, df_3)
    
  } else {df_to_rotate$Cell <- df_to_rotate$Cell-k*df_to_rotate$Time}
  
  
  
  
  # Returning initial column name if differs
  
  colnames(df_to_rotate)[which(names(df_to_rotate) == 'Cell')] <- initial_col_name
  
  return(df_to_rotate)
  
  
}



# For single plot replacing the column with new values (rotated)

replace_columns_in_dfs <- function(df_full, df_part) {
  
  inter <- intersect(colnames(df_full), colnames(df_part)[!grepl('([Tt]ime\\s?)', colnames(df_part))])
  
  
  df_full[which(colnames(df_full)==inter)] <- df_part[!grepl('([Tt]ime\\s?)', colnames(df_part))]
  
  return(df_full)
  
}












