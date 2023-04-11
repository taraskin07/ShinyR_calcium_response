necessary_packages <- c('tidyverse', 'pastecs', 'ggrepel', 'gganimate', 'plotly')
if (!require(necessary_packages, quietly = TRUE))
  install.packages(necessary_packages)

library(tidyverse)
library(readxl)
library(pastecs)

source("read_xls_table.R")


# Fixing the 'Time" column and rounding values
df_list <- reading_xls()
df_list <- lapply(df_list, time_col_name)


# Changing names of the column
df_340_ready <- rename_columns(df_list, 1)
df_380_ready <- rename_columns(df_list, 2)
df_ratio_ready <- rename_columns(df_list, 3)


# Evaluating ratio
# df_340_read <- df_340_ready[,!grepl("*ime *",names(df_340_ready))]

df_340_d <- df_340_ready[,2:length(df_340_ready)]
df_380_d <- df_380_ready[,2:length(df_380_ready)]
df_custom_ratio <- df_340_d/df_380_d

# Adding time column
df_custom_ratio$Time <- df_340_ready$Time
df_custom_ratio <- df_custom_ratio %>% 
  relocate(Time)
  


# Analysing data
bunch_of_dfs <- list(df_340_ready, df_380_ready, df_ratio_ready, df_custom_ratio)
names(bunch_of_dfs) <- c("t_340", "t_380", "t_ratio", "t_custom" )
# lapply(bunch_of_dfs, str)

basic_statistics <- function(df) {
  df <- df %>% 
    distinct(across(-Time))
  
  options(scipen=100)
  options(digits=3)
  res <- stat.desc(df)
  res_t <- as.data.frame(t(as.matrix(res)))
  return(res_t)
  
}


# Testing dimentions
dim_check <- function(list_of_df) {
  dims <- sapply(list_of_df, dim)
  unq <- apply(dims, MARGIN = 1, unique)
  if (length(unq) == 2) {
    return(unq[2]-1)
  } else {
    stop('Something wrong in dataset. Dimentions of the dataframes vary!')
  }
}



dim_check(bunch_of_dfs)





# Statistics tables
# stat_340 <- basic_statistics(df_340_ready)
# stat_380 <- basic_statistics(df_380_ready)
# stat_ratio <- basic_statistics(df_ratio_ready)
# stat_custom_ratio <- basic_statistics(df_custom_ratio)


names(bunch_of_dfs)


reading_bad_cell_numbers <- function(list_of_dataframes) {
  limit <- 
  bad_values <- c()
  val <- readline(prompt="Input numbers corresponding to the 'bad' cells: ")
  while (!grepl("[^[:digit:]\\.-]|^$",val)) {
    bad_values <- append(val, bad_values)
    val <- readline(prompt="Input another one or Enter to exit: ")
  }
  return(bad_values)
}

reading_bad_cell_numbers()

