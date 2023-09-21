ct <- read_excel("~/Current/CA Imaging checkpoints_V2/2023-04-13/No_Shaking_no_AVP_medium_replaced_24h/processed/CleanTable/2023-04-13-mpkCCD001-CleanTable.xlsx", 
                         sheet = "Ratio")

ct2 <- ct[c(1, 12, 101)]

dfts <- ct2
start_time <- 130
end_time <- 330

index1 <- min(which(dfts$Time >= start_time))
index2 <- max(which(dfts$Time <= end_time))

dfts[index1:index2,] 


which.max(dfts[index1:index2,][[1]])



dfts[index1:index2,][13, ]
max_indices <- apply(dfts[index1:index2,], 2, which.max)
max_indices


dfts[index1:index2,][14, ]


initial_indicies <- max_indices + index1 - 1
initial_indicies

dfts[40, 3]



indices_list <- initial_indicies[-1]
indices_list

lowest_value <- min(unlist(indices_list))


lowest_value

trace_name <- names(which.min(unlist(indices_list)))

trace_name

difference <- sapply(indices_list, function(x)
  x - lowest_value)
difference


# SHIFTING


reactive_df_to_shift <- read_excel("~/Current/CA Imaging checkpoints_V2/2023-04-28/No_Shaking_no_AVP/processed/CleanTable/2023-04-28-mpkCCD001-CleanTable.xlsx", 
                                               sheet = "Ratio")


empty_at_the_start <- read_excel("~/Current/CA Imaging checkpoints_V2/2023-04-29/10mM_LiCl_No_Shaking_no_AVP/processed/CleanTable/2023-04-29-mpkCCD004-CleanTable.xlsx", 
                                 sheet = "Ratio")
# Shifting curves (CCF)
CCF_matrix <- function(df_to_shift, lower, upper, max_lag) {
  start_time <- Sys.time()
  # Correcting Time column if not Time and not first in the dataframe
  df_time <- time_col_name(df_to_shift, name_only = T)
  
  # Subsetting according the range (lower-upper)
  subset_timerange <-
    as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))[-1]
  
  # Creating CCF matrix
  CCF_matrix = matrix(
    numeric(),
    nrow = ncol(subset_timerange),
    ncol = ncol(subset_timerange)
  )
  rownames(CCF_matrix) = colnames(subset_timerange)
  colnames(CCF_matrix) = colnames(subset_timerange)
  
  # For shiny R only
  # withProgress(message = "Calculating...", value = 0, {
  count <- 0
  len <- length(colnames(subset_timerange))
  
  for (columnName in colnames(subset_timerange)) {
    count <- count + 1
    for (rowName in colnames(subset_timerange)) {
      mtrx <- ccf(
        subset_timerange[columnName],
        subset_timerange[rowName],
        lag.max = max_lag,
        na.action = na.omit,
        plot = FALSE
      )
      data_table <- data.frame(ACF = mtrx$acf, Lag = mtrx$lag)
      lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
      CCF_matrix[rowName, columnName] = lag_for_max_acf
    }
    
    # For shiny R only
    # incProgress(1 / len, detail = paste("Processing trace", count))
  }
  
  # })
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  print(elapsed_time)
  return(CCF_matrix)
}

# # Start measuring time
# start_time <- Sys.time()
# 
# # Your program code goes here
# 
# # Stop measuring time
# end_time <- Sys.time()
# 
# # Calculate the elapsed time
# elapsed_time <- end_time - start_time
# 
# # Print the elapsed time
# print(elapsed_time)



# Shifting curves (CCF)
CCF_matrix2 <- function(df_to_shift, lower, upper, max_lag) {
  
  start_time <- Sys.time()
  # Correcting Time column if not Time and not first in the dataframe
  df_time <- time_col_name(df_to_shift, name_only = T)
  
  # Subsetting according the range (lower-upper)
  subset_timerange <-
    as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))[-1]
  
  # Creating CCF matrix
  CCF_matrix = matrix(
    numeric(),
    nrow = ncol(subset_timerange),
    ncol = ncol(subset_timerange)
  )
  rownames(CCF_matrix) = colnames(subset_timerange)
  colnames(CCF_matrix) = colnames(subset_timerange)
  
 
    count <- 0
    len <- length(colnames(subset_timerange))
    
    for (i in 1:ncol(subset_timerange)) {
      
      columnName <- colnames(subset_timerange)[[i]]
      count <- count + 1
      
      for (j in i:ncol(subset_timerange)) {
        
        rowName <- colnames(subset_timerange)[[j]]
        
        if (j == i) {
          
          CCF_matrix[rowName, columnName] = 0
          
        } else {

        mtrx <- ccf(
          subset_timerange[columnName],
          subset_timerange[rowName],
          lag.max = max_lag,
          na.action = na.omit,
          plot = FALSE
        )
        
        data_table <- data.frame(ACF = mtrx$acf, Lag = mtrx$lag)
        lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
        CCF_matrix[rowName, columnName] = lag_for_max_acf
        CCF_matrix[columnName, rowName] = -lag_for_max_acf
        }

      }
      
    }
    
    
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(elapsed_time)
    
  return(CCF_matrix)
}

# CHOOSING ANOTHER FILE
reactive_df_to_shift <- empty_at_the_start

lag_values_df <- CCF_matrix(
    reactive_df_to_shift,
    lower = 120,
    upper = 330,
    max_lag = 30
)

lag1 <- as.data.frame(lag_values_df)
rownames(lag1) <- rownames(lag_values_df)
write.xlsx(lag1, 'lag1.xlsx', rowNames = T)

lag_values_df2 <- CCF_matrix2(
  reactive_df_to_shift,
  lower = 120,
  upper = 330,
  max_lag = 30
)

lag2 <- as.data.frame(lag_values_df2)
rownames(lag2) <- rownames(lag_values_df2)
write.xlsx(lag2, 'lag2.xlsx', rowNames = T)

View(lag_values_df)
View(lag_values_df2)

identical(lag_values_df, lag_values_df2)


shift_with_CCF(reactive_df_to_shift,
               lag_values_df,
               max_lag = 30)


df_to_shift <- reactive_df_to_shift
CCF_matrix <- lag_values_df
df_time <- time_col_name(df_to_shift, name_only = T)

column_sums <- colSums(CCF_matrix)

View(column_sums)
left_trace_column <- names(which(column_sums == min(column_sums)))[[1]]
left_trace_column
min_column_sums <- min(column_sums)
min_column_sums




# EMPTY VALUES AT START

empty_at_the_start <- read_excel("~/Current/CA Imaging checkpoints_V2/2023-04-29/10mM_LiCl_No_Shaking_no_AVP/processed/CleanTable/2023-04-29-mpkCCD004-CleanTable.xlsx", 
                                   sheet = "Ratio")


lag_values_df <- CCF_matrix2(
  empty_at_the_start,
  lower = 120,
  upper = 330,
  max_lag = 30
)

View(lag_values_df)

df_time <- time_col_name(empty_at_the_start, name_only = T)
CCF_matrix <- lag_values_df
max(CCF_matrix)



# Known index
single_index <- which.min(CCF_matrix)
single_index
# Find row and column names
matrix_dim <- dim(CCF_matrix)
num_rows <- matrix_dim[1]
num_cols <- matrix_dim[2]

row_index <- (single_index - 1) %/% num_cols + 1
col_index <- (single_index - 1) %% num_cols + 1

row_name <- rownames(CCF_matrix)[row_index]
row_name
col_name <- colnames(CCF_matrix)[col_index]
col_name

column_sums <- colSums(CCF_matrix)
left_trace_column <- names(which(column_sums == min(column_sums)))[[1]]
left_trace_column
min_column_sums <- min(column_sums)
min_column_sums


for (nm in colnames(df_time)[-1]) {
  df_time[[nm]] <- shift(df_time[[nm]],
                         n = CCF_matrix[nm, left_trace_column])
}

df_time

named_list <- list() 

for (nm in colnames(df_time)[-1]) {
  n = CCF_matrix[nm, left_trace_column]
  
  named_list[[nm]] <- n 
  }

max_shift <- max(unlist(named_list))


list_subtracted <- lapply(named_list, function(x) x - max_shift)

list_subtracted



CCF_matrix

negative_zero_cols <- colnames(CCF_matrix)[colSums(CCF_matrix >= 0) == 0]
negative_zero_cols









