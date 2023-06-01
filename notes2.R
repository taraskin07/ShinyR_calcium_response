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











min_time =0
max_time = 500
dftmrng <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002-CleanTable.xlsx", sheet = 'ratio')
dftmrng<- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", sheet = 'Ratio')

column_names <- colnames(dftmrng)
column_names

time_col <- '((T|t)ime\\s?)'
time_col
time_column <- column_names[grepl(time_col, column_names)]
time_column

grep(time_col, column_names)

df_time <- dftmrng
colnames(df_time)[grep(time_col, column_names)] <-'Time'

subset_timerange <- subset(df_time, (Time >= min_time & Time <= max_time))
subset_timerange


list_of_names <- colnames(subset_timerange)
list_of_names
match <- paste0('(\\D|0+)', 30, '($|\\s)')
match
reference <- column_names[grepl(match, list_of_names)]
main_cell <- column_names[grepl(match, list_of_names)]
main_cell
reference

cell_index <- grep(match, colnames(subset_timerange))
cell_index
colnames(subset_timerange)[cell_index]


coln_df <- list_of_names[!grepl(time_col, list_of_names)]
coln_df

# Finding the cell with the earliest maximum, skipping the Time column (coln_df instead of list_of_names)
for (cell in coln_df) {
  
    if (shift(subset_timerange[,cell], subset_timerange[,reference]) < 0) {
    
    reference <- cell
    }
  
}


reference


# Shifting main series to the left in order to correlate with the reference (with the earliest maximum)
# Cross-correlation function
mtrx <- ccf(subset_timerange[, main_cell], subset_timerange[,reference], na.action=na.omit, plot=FALSE)


# Dataframe with all the necessary information
data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)



# Position of the maximum CCF (ACF) value = lag to choose
lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]

lag_for_max_acf <- shift(subset_timerange[, main_cell], subset_timerange[,reference])
lag_for_max_acf


# Shifting initial dataframe column, related to the main_cell, that was chosen by the operator
shifted_main_cell_values <- subset_timerange[, main_cell][(lag_for_max_acf+1):nrow(subset_timerange[,main_cell]),]


shifted_main_cell_values


ggplotly_render(df_time[c('Time', 'cell-030', 'cell-136')])
ggplotly_render(df_time)




result_df <-data.frame(Time = dftmrng[, list_of_names[grepl(time_col, list_of_names)]])
result_df


for (cell in coln_df) {

  lag_for_max_acf <- shift(subset_timerange[, cell], shifted_main_cell_values)
  print(lag_for_max_acf)
  cell
  shifted_cell_values <- dftmrng[, cell][(lag_for_max_acf+1):nrow(dftmrng[, cell]),]
  result_df <- as.data.frame(cbind.fill(result_df, shifted_cell_values))
}


result_df


my_result <- na.omit(result_df)

ggplotly_render(my_result)

my_result

shift(my_result$`cell-030`, my_result$`cell-134`)


