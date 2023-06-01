dfr <- read_excel("files/2022-02-25-mpkCCD002-Raw(UTP).xlsx")
dfr <- read_excel("files/2023-04-25-mpkCCD004-CleanTable.xlsx")
parse_str <- colnames(dfr)
parse_str



  
my_expression <- "#(\d+)"
regmatches(parse_str, regexpr("^#(\\d+)", parse_str))
tf <- regmatches(parse_str, regexec("^#(\\d+)\\s\\((\\d+)\\)", parse_str))
tf
typeof(tf)
tf[[2]][3]

rs <- regmatches(parse_str, regexec("^#(\\d+)", parse_str))
unlist(rs)
rd <- sapply(rs, "[", 2) 
rn <- rd[grepl('[0-9]', rd)]

rn

?gsub()
?regexec
  sub("^#(\\d+)", regexec("^#(\\d+)", parse_str), parse_str)

  
  
ggplotly_render(dfr)

list_of_names <- colnames(dfr)
list_of_names
time_col <- '((T|t)ime\\s?)'
coln_df <- list_of_names[!grepl(time_col, list_of_names)]
coln_df
match <- paste0('\\D', 3, '$')
reference <- list_of_names[grepl(match, list_of_names)]
main_cell <- list_of_names[grepl(match, list_of_names)]
main_cell

for (cell in coln_df) {
  
  if (shift(dfr[,cell], dfr[,reference], max_lag=50) < 0) {
    
    reference <- cell
  }
  
}
reference

mtrx <- ccf(dfr[,main_cell], dfr[,reference], lag = 50, na.action=na.omit, plot=FALSE)

data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)

lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
lag_for_max_acf

shifted_main_cell_values <- dfr[, main_cell][(lag_for_max_acf+1):length(dfr[, main_cell])] 
nrow(dfr[, main_cell][(lag_for_max_acf+1):nrow(dfr[,main_cell]),])

nrow(dfr[,main_cell])

shifted_cell2 <- dfr[, main_cell][(lag_for_max_acf+1):nrow(dfr[,main_cell]),]
shifting_curves(dfr, shifted_cell2, max_lag=50)
length(shifted_cell_values)

length(shifted_cell_values) <- nrow(df[, cell])


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
  
  match <- paste0('\\D', main_cell_number, '$')
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


shifted_cell <- finding_shifted_curve(dfr, 3, max_lag=50)


list_of_names
excl_col <- list_of_names[!grepl(time_col, list_of_names)]
excl_col
result_df <-data.frame(Time = dfr[, list_of_names[grepl(time_col, list_of_names)]])

mtrx <- ccf(dfr[, 'cell-72'], shifted_cell, lag = 50, na.action=na.omit, plot=FALSE)
data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
lag_for_max_acf
shifted_cell_values <- dfr[, 'cell-72'][(lag_for_max_acf+1):nrow(dfr[, 'cell-72']),]
result_df <- as.data.frame(cbind.fill(result_df, shifted_cell_values))
typeof(result_df)






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



my_result <- na.omit(shifting_curves(dfr, shifted_cell, max_lag=50))

ggplotly_render(my_result)
