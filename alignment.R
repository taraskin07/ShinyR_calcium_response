shift <- function(maximum_after, maximum_before, max_lag=50) {
  
  mtrx <- ccf(maximum_after, maximum_before, lag=max_lag, na.action=na.omit, plot=FALSE)
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  return(lag_for_max_acf)
}




finding_shifted_curve <- function(df, main_cell_number, max_lag) {
  
  list_of_names <- colnames(df)
  time_col <- '((T|t)ime\\s?)'
  coln_df <- list_of_names[!grepl(time_col, list_of_names)]
  
  match <- paste0('(\\D|0+)', main_cell_number, '$')
  reference <- list_of_names[grepl(match, list_of_names)]
  main_cell <- list_of_names[grepl(match, list_of_names)]
  
  for (cell in coln_df) {
    
    if (shift(df[,cell], df[,reference], max_lag=max_lag) < 0) {
      
      reference <- cell
    }
    
  }
  
  mtrx <- ccf(df[,main_cell], df[,reference], lag = max_lag, na.action=na.omit, plot=FALSE)
  
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
  
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  shifted_main_cell_values <- df[, main_cell][(lag_for_max_acf+1):nrow(df[,main_cell]),]
  
  return(shifted_main_cell_values)
  
}




cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}





shifting_curves <- function(df, shifted_reference, max_lag) {
  
  list_of_names <- colnames(df)
  time_col <- '((T|t)ime\\s?)'
  excl_col <- list_of_names[!grepl(time_col, list_of_names)]
  result_df <-data.frame(Time = df[, list_of_names[grepl(time_col, list_of_names)]])
  
  for (cell in excl_col) {
    mtrx <- ccf(df[, cell], shifted_reference, lag = max_lag, na.action=na.omit, plot=FALSE)
    data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
    lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
    
    shifted_cell_values <- df[, cell][(lag_for_max_acf+1):nrow(df[, cell]),]
    result_df <- as.data.frame(cbind.fill(result_df, shifted_cell_values))
  }
  
  res <- result_df
  return(res)
  
}

dfr <- read_excel("~/calcium_data/timeseries/2023-04-05/No_Shaking_no_AVP/processed/CleanTable/2023-04-05-mpkCCD006-CleanTable.xlsx")


dfr<- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-CleanTable.xlsx", sheet='ratio')



ggplotly_render(dfr)

my_dfr <- dfr %>% 
  add_column(Average = rowMeans(dfr[-1]))

my_dfr2 <- my_dfr[,c('Time', 'Average')]
ggplotly_render(my_dfr2)





shifted_cell <- finding_shifted_curve(dfr, 12, max_lag=50)

my_result <- na.omit(shifting_curves(dfr, shifted_cell, max_lag=50))







my_mean <- my_result %>% 
  add_column(Average = rowMeans(my_result[-1]))

write_xlsx(my_mean,"Res_Average.xlsx")

my_mean2 <- my_mean[,c('Time', 'Average')]
ggplotly_render(my_mean2)

ggplotly_render(my_result)


