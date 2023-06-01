
# Basic function to determine lag and it's sign between two series of values
shift <- function(maximum_after, maximum_before) {
  
  # Cross-correlation function, omits any NA values and skip plot rendering
  mtrx <- ccf(maximum_after, maximum_before, na.action=na.omit, plot=FALSE)
  
  #To get data from resulting table - new dataframe is constructed with all the necessary information
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
  
  #Lag that is corresponded to the maximum CCF (ACF) value
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  
  # Can be >0 (maximum_after, maximum_before) or <0 (maximum_before, maximum_after) or 0 (maximum_before = maximum_after)
  return(lag_for_max_acf)
}






# Function to find curve with maximum that is closer to the left (earlier time)
finding_shifted_curve <- function(df, main_cell_number, lower, upper) {
  
  
  
  # Obtaining list of dataframe column names
  list_of_names <- colnames(df)
  
  # Regular expression to identify Time column
  time_col <- '((T|t)ime\\s?)'
  
  # Removing Time column from the list coln_df
  coln_df <- list_of_names[!grepl(time_col, list_of_names)]
  
  
  # Regular expression to identify cell with the number input given
  match <- paste0('(\\D|0+)', main_cell_number, '$')
  
  # Current reference cell which probably would be changed (it's number) during the process of finding the one with the earliest maximum
  reference <- list_of_names[grepl(match, list_of_names)]
  
  # The cell, that was chosen by the operator as a reference
  main_cell <- list_of_names[grepl(match, list_of_names)]
  
  # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
  
  
  # Finding the cell with the earliest maximum, skipping the Time column (coln_df instead of list_of_names)
  for (cell in coln_df) {
    
    if (shift(df[,cell], df[,reference], max_lag=max_lag) < 0) {
      
      reference <- cell
    }
    
  }
  
  
  # Shifting main series to the left in order to correlate with the reference (with the earliest maximum)
  # Cross-correlation function
  mtrx <- ccf(df[,main_cell], df[,reference], lag = max_lag, na.action=na.omit, plot=FALSE)
  
  # Dataframe with all the necessary information
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
  
  # Position of the maximum CCF (ACF) value = lag to choose
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  # Shifting initial dataframe column, related to the main_cell, that was chosen by the operator
  shifted_main_cell_values <- df[, main_cell][(lag_for_max_acf+1):nrow(df[,main_cell]),]
  
  # Series of values, shifted to the left, without NA values (shorter series instead)
  return(shifted_main_cell_values)
  
}