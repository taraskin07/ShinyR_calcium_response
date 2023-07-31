

all_cells2_CleanTable <- read_excel("~/Rprojects/Test_files/all_cells2-CleanTable.xlsx", 
                                    sheet = "Ratio")

colorPalette <- randomColor(count = 2000, hue = 'random', luminosity = 'bright')


all <- ggplotly_render(all_cells2_CleanTable, 
                rcolor = color_palette(all_cells2_CleanTable, colorPalette),
                sorting = 'Native')
all


cell042 <- display_single_plot(all_cells2_CleanTable, 'cell-042')
cell042
df_to_shift[['cell-042']]
df_to_shift['cell-042'] <- shift(df_to_shift[['cell-042']], -20)
gggg <- df_to_shift['cell-042']



cell042_2 <- display_single_plot(df_to_shift, 'cell-042')
cell042_2




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


lower <- 0
upper <- 890
max_lag <- 40

dt_to_shift <- all_cells2_CleanTable
# PART
df_to_shift <- all_cells2_CleanTable[, c('Time',
                                        'cell-042',
                                        'cell-039',
                                        'cell-049', 
                                        'cell-088', 
                                        'cell-117', 
                                        'cell-195', 
                                        'cell-228')]

ggplotly_render(df_to_shift, 
                rcolor = color_palette(df_to_shift, colorPalette),
                sorting = 'Native')
library('zoo')
library('data.table')


ka <- 30

diff(rollmeandf[, 'Y'])
second_dif <- diff(sign(diff(rollmeandf[, 'Y'])))
second_dif

Compose <- function(x, ...)
{
  lst <- list(...)
  for(i in rev(seq_along(lst)))
    x <- lst[[i]](x)
  x
}


# time_for_maximum <- function(listElement, dfts, k) {
#   
#     for (id in 1:length(listElement)) {
#       
#       subset(dfts[listElement[id]:listElement[id]+k]
#     }
#   
# }

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

list_of_ids <- finding_local_maximum(df_to_shift)

lowest_value <- min(unlist(list_of_ids))
trace_name <- names(which.min(unlist(list_of_ids)))

lowest_value_index <- which.min(unlist(list_of_ids))[[1]]

unlist(list_of_ids)

difference <- -1*sapply(list_of_ids, function(x) x-lowest_value)
difference


df_shifted <- df_to_shift[-1]

for (element in names(difference)) {
  
  
  
  print(difference[[element]])
}



difference['cell-042']

names(difference)



view(df_shifted)

df_shifted2 <- lapply(df_shifted, shift, difference)
View(df_shifted2)
df_shifted <- cbind(df_to_shift[1], df_shifted)

resultpl <- ggplotly_render(df_shifted, 
                       rcolor = color_palette(df_shifted, colorPalette),
                       sorting = 'Native')
resultpl





dfts <- df_to_shift[5]
k=30
means <- as_tibble(rollmean(dfts, k=k))
means

max_values <- as_tibble(rollmax(dfts, k=k))

derivative2 <- lapply(means, Compose, diff, sign, diff)

indexes <- lapply(derivative2, function(x) which(x==-2))
indexes
maxValues <- indexes
names(max_values)


for (name in names(max_values)) {
  value <- max_values[[name]]
  index <- indexes[[name]]
  index_for_max <- which.max(value[index])
  maxValues[[name]] <- index[index_for_max]
}

index

max_values[["cell-088"]]
indexes[["cell-088"]]
maxValues[indexes[["cell-088"]]]
value[index]
max(value[index])
index_for_max <- which.max(value[index])
index_for_max
which.max(value[index])
maxValues

values <- as_tibble(rollmean(dfts[-1], k=k))

indexes <- as_tibble(rollmax(dfts[-1], k=k))



mn <- df_to_shift[5]
mn
rlmn <- as_tibble(rollmean(mn, k = ka))
rlmn

rlmax <- as_tibble(rollmax(mn, k = ka))
rlmax[75,]
View(rlmax)

nrow(rlmn)
pl <- data.frame(Time = seq.int(from = 0, by = 5, length.out = nrow(rlmn)), Cell = rlmn)
ggplotly_render(pl)
pl_init <- data.frame(Time = seq.int(from = 0, by = 5, length.out = nrow(mn)), Cell = mn)
ggplotly_render(pl_init)



mtrx <- ccf(mn, rlmn, lag.max = max_lag, na.action=na.omit, plot=FALSE)
data_table <- data.frame(ACF=mtrx$acf, Lag=mtrx$lag, N=mtrx$n.used)
lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
lag_for_max_acf

pl$Time[which.max(pl$cell.039)]
pl_init$Time[which.max(pl_init$cell.039)]








nrow(dt_to_shif)





cell39col <- dt_to_shif[['cell-039']]
x2 <- dt_to_shif['cell-039']
x2
x
x_6 <- x2[1:6,]


nrow(x_6)


length(rollmax(x_6, k = 10))

rollmax(x_6, k = 3)
rollmean(x_6, k = 3)
rollmedian(x_6, k = 3)


x <- dt_to_shif['Time']
x <- x[1:176,]
y <- rollmean(cell39col, k=5)
length(y)
nrow(x)
rollmeandf <- data.frame(X = x, Y = y)
ggplotly_render(rollmeandf, 
                rcolor = randomColor(count = 2, hue = 'random', luminosity = 'bright'),
                sorting = 'Native')


diff(rollmeandf[, 'Y'])
second_dif <- diff(sign(diff(rollmeandf[, 'Y'])))
second_dif
second_dif[78]

dt_to_shif[84, ]

local_max <- which(second_dif==-2)

c39 <- diff(dt_to_shif[['cell-039']])
print(c39)

local_max


mx <- 0
for (id in 1:length(local_max)) {
  cmx <- max(mn[(local_max[id] - ka):(local_max[id] + ka),])
  if (is.na(cmx)) {
    mx <- mx} else {
      if (cmx > mx) {
        mx <- cmx
      }
      
    }
}
 
print(mx) 


dt_to_shif[84, ]








finding_reference_curve_for_shifting_others






shifted_main_cell_values <- finding_shifted_curve(dt_to_shift, 
                      main_cell_number = 195, 
                      lower = start_t_shift, 
                      upper = end_t_shift, 
                      max_lag = max_lag)


lag_data <- data.frame(A = character(), B = numeric())


colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])

lag_data

shifted_info <- shifting_curves_info(lag_data, dt_to_shift, 
                                     shifted_main_cell_values, 
                                     lower = start_t_shift, 
                                     upper = end_t_shift, 
                                     max_lag = max_lag)



cell <- colnames(dt_to_shift)[31]

main_cell_number <- str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
main_cell_number



shifted_main_cell_values <- finding_shifted_curve(dt_to_shift, main_cell_number, start_t_shift, end_t_shift, max_lag)






















lag_values_df <- shifted_info


