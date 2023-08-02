

all_cells2_CleanTable <- read_excel("~/Rprojects/Test_files/all_cells2-CleanTable.xlsx", 
                                    sheet = "Ratio")

colorPalette <- randomColor(count = 2000, hue = 'random', luminosity = 'bright')


all <- ggplotly_render(all_cells2_CleanTable, 
                rcolor = color_palette(all_cells2_CleanTable, colorPalette),
                sorting = 'Native')
all


cell042 <- display_single_plot(all_cells2_CleanTable, 'cell-042')
cell042
cell049 <- display_single_plot(all_cells2_CleanTable, 'cell-049')
cell049
cell088 <- display_single_plot(all_cells2_CleanTable, 'cell-088')
cell088
cell117 <- display_single_plot(all_cells2_CleanTable, 'cell-117')
cell117
cell195 <- display_single_plot(all_cells2_CleanTable, 'cell-195')
cell195
cell228 <- display_single_plot(all_cells2_CleanTable, 'cell-228')
cell228
cell039 <- display_single_plot(all_cells2_CleanTable, 'cell-039')
cell039


lower <- 170
upper <- 785
max_lag <- 50

df_to_shift_alt <- all_cells2_CleanTable[, c('Time',
                                         'cell-228',
                                         'cell-042',
                                         'cell-049', 
                                         'cell-088', 
                                         'cell-117', 
                                         'cell-195', 
                                         'cell-039')]

df_to_shift_alt$Time[8] <- 7

timeStep <- unique(diff(df_to_shift_alt$Time))
timeStep
length(unique(diff(diff(df_to_shift_alt$Time)))) == 1

dt_CCF <- CCF_matrix(df_to_shift_alt, lower, upper, max_lag)
View(dt_CCF)





dt_to_shift <- shift_with_CCF(all_cells2_CleanTable, dt_CCF, max_lag)

dt_CCF <- CCF_matrix(all_cells2_CleanTable, lower, upper, max_lag)
View(dt_CCF)
dt_to_shift <- shift_with_CCF(all_cells2_CleanTable, dt_CCF, max_lag)


df_to_shift <- shift_to_match_maximum(all_cells2_CleanTable)
# CCF
ggplotly_render(dt_to_shift, 
                rcolor = color_palette(dt_to_shift, colorPalette),
                sorting = 'Native')

averCCF <- average_curve(dt_to_shift)

ggplotly_render(averCCF)
# MAXIMUMS
ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')

averMax <- average_curve(df_to_shift)

ggplotly_render(averMax)

df_to_shift2 <- shift_with_CCF(df_to_shift, lower, upper, max_lag = 50)
ggplotly_render(df_to_shift2, 
                rcolor = color_palette(df_to_shift2, colorPalette),
                sorting = 'Native')

averCCF2 <- average_curve(df_to_shift2)

ggplotly_render(averCCF2)



# PART
df_to_shift <- all_cells2_CleanTable[, c('Time',
                                        'cell-042',
                                        'cell-039',
                                        'cell-049', 
                                        'cell-088', 
                                        'cell-117', 
                                        'cell-195', 
                                        'cell-228')]
shift_to_match_maximum(df_to_shift)


ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')



df_time <- time_col_name(df_to_shift, name_only = T)

subset_timerange <- as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))[-1]





CCF_matrix = matrix(numeric(), 
                     nrow = ncol(subset_timerange), 
                     ncol = ncol(subset_timerange))

rownames(CCF_matrix) = colnames(subset_timerange)
colnames(CCF_matrix) = colnames(subset_timerange)
View(CCF_matrix)


for (columnName in colnames(subset_timerange)) {

  for (rowName in colnames(subset_timerange)) {
  mtrx <- ccf(subset_timerange[columnName], 
              subset_timerange[rowName], 
              lag.max = max_lag, 
              na.action=na.omit, 
              plot=FALSE)
  data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag)
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  CCF_matrix[rowName, columnName] = lag_for_max_acf
  }
}

View(CCF_matrix)

# Columns contain information about a trace that should be the reference (CCF < 0)
# So maximum in CCF_matrix == reference
# Rows for the case when CCF > 0
# So minimum in CCF_matrix == reference


if (max(CCF_matrix) == max_lag) {


print('Lag value for some trace is the same as the maximum lag value entered!
      You should consider to increase the maximum lag value or use another algorithm at first!')
  }



column_sums <- colSums(CCF_matrix)
column_sums
# row_sums <- rowSums(CCF_matrix)
# row_sums

left_trace_column <- names(which(column_sums == min(column_sums)))
# 
# left_trace_row <- names(which(row_sums == min(row_sums)))

left_trace_column
# left_trace_row

min_column_sums <- min(column_sums)

# min_row_sum <- min(row_sums)

min_column_sums
# min_row_sum


for (nm in colnames(df_time)[-1]) {
  
  df_time[[nm]] <- shift(df_time[[nm]], 
                         n = CCF_matrix[nm, left_trace_column])
  }


# if (min(column_sums) <= min(row_sums)) {
#   
#   for (nm in colnames(df_time)[-1]) {
#     
#     df_time[[nm]] <- shift(df_time[[nm]], 
#                            n = CCF_matrix[nm, left_trace_column])}
#   
#   } else {
#     
#   for (nm in colnames(df_time)[-1]) {
#       
#     df_time[[nm]] <- shift(df_time[[nm]], 
#                            n = CCF_matrix[left_trace_row, nm])}
#   
# }


ggplotly_render(df_time, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')


aver3 <- average_curve(df_time)

ggplotly_render(aver3)







ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')

View(CCF_matrix)


ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')
































# Function finds the response-specific maximum for each trace 
# and creates resulting list of indexes = response-specific maximums

finding_local_maximum <- function(ts_table, k = 30) {
  
  # Correcting Time column if not Time and not first in the dataframe
  dfts <- time_col_name(ts_table, name_only = T)
  
  # Values from the dataframe without Time column
  values <- as_tibble(dfts[-1])
  
  # Moving average for each trace, k stands for window based on index value
  means <- as_tibble(rollmean(dfts[-1], k=k))
  
  # Moving maximum with the same window value
  max_values <- as_tibble(rollmax(dfts[-1], k=k))
  
  # Second derivative to find local maximum regions
  derivative2 <- lapply(means, Compose, diff, sign, diff)
  
  # Local maximum regions (starting point)
  indexes <- lapply(derivative2, function(x) which(x==-2))
  
  # Output list of traces with time values, representing maximum - response
  maxValues <- list()
  
  # Creating output list, for each trace name in moving maximum dataframe
  for (name in names(max_values)) {
    
    # sequence of maximum values for the current trace
    value <- max_values[[name]]
    
    # sequence of indexes, representing the starting points of local maximum regions (current trace)
    index <- indexes[[name]]
    
    # finding which index gives the maximum value, so local maximum is the response
    index_for_max <- which.max(value[index])
    
    # Slice for region of interest
    row_indexes_for_max <- index[index_for_max]:(index[index_for_max]+k)
    
    # Obtaining values for the current trace
    current_column <- values[[name]]
    
    # Index, that represents maximum value for the response
    maximum_index <- index[index_for_max] + which.max(current_column[row_indexes_for_max]) - 1
    
    # Saving in the resulting dataframe
    maxValues[[name]] <- maximum_index
  }
  
  return(maxValues)
  
}



shift_to_match_maximum <- function(df_to_shift) {
  
  
  # Generating list of indexes = response-specific maximums by custom function
  list_of_ids <- finding_local_maximum(df_to_shift)

  # Establishing the earliest maximum among all the traces
  lowest_value <- min(unlist(list_of_ids))

  # And its name
  trace_name <- names(which.min(unlist(list_of_ids)))

  # Creating list of lag values as compared to the one with the earliest maximum
  difference <- sapply(list_of_ids, function(x) x-lowest_value)
  print(difference)

  for (element in names(difference)) {
    
    df_to_shift[[element]] <- shift(df_to_shift[[element]], 
                                    n=difference[[element]], 
                                    type = 'lead')
  
  }

  return(df_to_shift)
}





ggplotly_render(all_cells2_CleanTable, 
                rcolor = color_palette(all_cells2_CleanTable, colorPalette),
                sorting = 'Native')

df_shifted2 <- shift_to_match_maximum(all_cells2_CleanTable)

ggplotly_render(df_shifted2, 
                rcolor = color_palette(all_cells2_CleanTable, colorPalette),
                sorting = 'Native')
aver2 <- average_curve(df_shifted2)

ggplotly_render(aver2)






ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')

df_shifted <- shift_to_match_maximum(df_to_shift)

ggplotly_render(df_shifted, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')




aver <- average_curve(df_shifted)

ggplotly_render(aver)





