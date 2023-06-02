
# Basic function to determine lag and it's sign between two series of values
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
  print(paste0('1    ',df_time))
  # Obtaining list of dataframe column names
  list_of_names <- colnames(df_time)
  print(paste0('2    ',list_of_names))
  # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
  subset_timerange <- as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))
  print(paste0('3    ',subset_timerange))
  # Regular expression to identify cell with the number input given
  match <- paste0('(\\D0*)', main_cell_number, '($|\\s)')
  print(paste0('4    ',match))
  # Current reference cell which probably would be changed (it's number) during the process of finding the one with the earliest maximum
  reference <- list_of_names[grepl(match, list_of_names)]
  
  if (length(reference)>1) {stop("More than one column with similar name!")}
  print(paste0('reference    ',reference))
  # The cell, that was chosen by the operator as a reference
  main_cell <- list_of_names[grepl(match, list_of_names)]
  print(paste0('main_cell    ',main_cell))
  # Excluding time column from column names list
  coln_df <- list_of_names[list_of_names != "Time"]
  print(coln_df)
  # Finding the cell with the earliest maximum, skipping the Time column (coln_df instead of list_of_names)
  for (cell in coln_df) {

    if (shift(subset_timerange[, cell], subset_timerange[, reference], max_lag) < 0) {
      print(paste0('7    ',cell))
      reference <- cell

    }

  }


  # Shifting main series to the left in order to correlate with the reference (with the earliest maximum)
  # Position of the maximum CCF (ACF) value = lag to choose
  lag_for_max_acf <- shift(subset_timerange[, main_cell], subset_timerange[, reference], max_lag)
  print(lag_for_max_acf)
  print(length(subset_timerange[, main_cell]))

  # Shifting initial dataframe column, related to the main_cell, that was chosen by the operator
  shifted_main_cell_values <- subset_timerange[, main_cell][(lag_for_max_acf+1):length(subset_timerange[, main_cell])]

  # # Series of values, shifted to the left, without NA values (shorter series instead) from the main cell column that was chosen
  return(shifted_main_cell_values)
  
}

shifting_curves <- function(df, shifted_main_cell_values, lower, upper, max_lag) {
  
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

  
  
  for (cell in coln_df) {
    
    lag_for_max_acf <- shift(subset_timerange[, cell], shifted_main_cell_values, max_lag)
    
    if (lag_for_max_acf < 0) {
      print(paste0('The current cell: ', cell, ' is shifted to the left from the reference. The lag is: ', lag_for_max_acf))
      stop("Try to repeat the procedure and take this cell as a reference or extend the time window and decrease the maximum lag!")
      
    } else {  
      
      shifted_cell_values <- df[, cell][(lag_for_max_acf+1):length(df[, cell]),]
      result_df <- as.data.frame(cbind.fill(result_df, shifted_cell_values))}
    
  }

  return(result_df)
}


dftmrng <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002-CleanTable.xlsx", sheet = 'ratio')

ggplotly_render(dftmrng)

low=120
high=350
lagv=20
celln=42



shifted_main_cell_values <- finding_shifted_curve(dftmrng, celln, low, high, lagv)
shifted_main_cell_values



sc <- shifting_curves(dftmrng, shifted_main_cell_values, low, high, lagv)
sc
ggplotly_render(sc)



my_result <- na.omit(sc)
ggplotly_render(my_result[, c('Time', 'cell-042')])
ggplotly_render(my_result)


my_mean <- my_result %>% 
  add_column(Average = rowMeans(my_result[-1]))

my_mean[, c('Time', 'Average')]
ggplotly_render(my_mean[, c('Time', 'Average')])



