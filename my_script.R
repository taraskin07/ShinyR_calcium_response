necessary_packages <- c('tidyverse', 'ggrepel', 'gganimate', 'plotly')
if (!require(necessary_packages, quietly = TRUE))
  install.packages(necessary_packages)

library(tidyverse)
# library(dplyr)
# library(stringr)
library(readxl)

path <- '2022-02-25-mpkCCD002-Raw(UTP).xlsx'

if(!exists("reading_xls", mode="function")) source("read_xls_table.R")


# path %>% excel_sheets() %>% 
#   set_names() %>% 
#   map_dfr(read_excel, path=path) 
# 
# sheets <- excel_sheets(path); sheets

# 
# df_340 <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='340')
# if (colnames(df_340)[1] == 'Time [s]') {colnames(df_340)[1] = "Time"}
# df_340$Time <-  round(df_340$Time)
# 
# df_380 <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='380')
# if (colnames(df_380)[1] == 'Time [s]') {colnames(df_380)[1] = "Time"}
# df_380$Time <- round(df_380$Time)
# 
# df_ratio <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='Ratio')
# if (colnames(df_ratio)[1] == 'Time [s]') {colnames(df_ratio)[1] = "Time"}
# df_ratio$Time <- round(df_ratio$Time)




# List of dataframes
df_340 <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='340')
df_380 <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='380')
df_ratio <- read_excel('2022-02-25-mpkCCD002-Raw(UTP).xlsx', sheet='Ratio')
df_list <- list(df_340, df_380, df_ratio)


# Function to fix the 'Time' column and rounding it's values
time_col_name <- function(datafr) {
  if (colnames(datafr)[1] == "Time [s]") {
    colnames(datafr)[1] =  "Time"
  }
  datafr$Time <- round(datafr$Time)
  return(datafr)
}


# Fixing the 'Time" column and rounding values
df_list <- lapply(df_list, time_col_name)

# 
# View(df_list[[3]])



# Changing names of the column

df_340_ready <- df_list[[1]] %>% 
  rename_with(.cols = contains("#"), # selects only the data columns
              ~ paste0(
                "cell-",  
                stringr::str_sub(.x, 2, 3) # first 2 digits after #
              )
  )


df_380_ready <- df_list[[2]] %>% 
  rename_with(.cols = contains("#"), # selects only the data columns
              ~ paste0(
                "cell-",  
                stringr::str_sub(.x, 2, 3) # first 2 digits after #
              )
  )

df_ratio_ready <- df_list[[3]] %>% 
  rename_with(.cols = contains("#"), # selects only the data columns
              ~ paste0(
                "cell-",  
                stringr::str_sub(.x, 2, 3) # first 2 digits after #
              )
  )

df_340_ready
df_380_ready
df_ratio_ready




# Plotting initial data
library(ggplot2)
library(ggrepel)
library(gganimate)
library(plotly)

#Convert from long format to tidy format
#The conversion assumes that the first column is the Time data and remaining columns the measured Value of different Objects
# df_tidy_340 <- gather(df_340_ready, Object, Value,-Time)

df_tidy_340 <- df_340_ready %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "r.u.")


df_tidy_340

ggplot(df_tidy_340, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+
  geom_point(size = 0.2) + theme(legend.position = "none")

# Part of plot to analyse
df_tidy_340_part <- df_tidy_340 %>% 
  filter(Time > 100 & Time < 300)

df_tidy_340_part

p <- ggplot(df_tidy_340_part, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+
  geom_point(size = 0.2) + theme(legend.position = "none")

ggplotly(p)



if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github('larslau/pavpop')

library(pavpop)
?pavpop


# Ratio

df_tidy_ratio <- df_ratio_ready %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "r.u.")

df_vals <- df_ratio_ready[2:length(df_ratio_ready)]
y <- as.list(df_vals)
y
length(y)

t <- df_ratio_ready$Time
length(t)

t_range <- c(0, 600)

# Set up basis function
kts <- seq(t_range[1], t_range[2], length = 15)
basis_fct <- make_basis_fct(kts = kts, type = 'increasing', intercept = TRUE,
                            control = list(boundary = t_range))

# Set up warp function
tw <- seq(t_range[1], t_range[2], length = 6)
warp_fct <- make_warp_fct('smooth', tw, control = list(wright = 'extrapolate'))
mw <- attr(warp_fct, 'mw')

pavpop(y, t, basis_fct, warp_fct, amp_cov = NULL, warp_cov = NULL,
       amp_fct = NULL, warped_amp = FALSE, iter = c(5, 5),
       parallel = list(n_cores = 1, parallel_likelihood = FALSE),
       use_warp_gradient = FALSE, warp_optim_method = "CG",
       homeomorphisms = "no", like_optim_control = list())

















#   
#   namecol <- str_replace(namecol, "(\\d+).*?(\\d+)$", "election_\\2_\\1")
# namecol
# 
# 
# 
# colnames(df_ratio)
# 
# df_340_sbs <- subset(df_340, select=1:4)[1:4, ]
# df_380_sbs <- subset(df_380, select=1:4)[2:6, ]
# df_ratio <- subset(df_ratio, select=1:4)[1:5, ]
# 
# vectr <- list(df_340_sbs, df_380_sbs, df_ratio)
# vectr[2]
# 
# df_res <-  merge(df_340_sbs,df_380_sbs, by="Time")
# df_res3 <- merge(df_res,df_ratio, by="Time")
# df_res3
# 
# 
# 
# colnames(df_res3)
# val_col <- colnames(df_res3);val_col
# rdr <- order(val_col);rdr
# order_vals <- c(1, rdr[-length(rdr)])
# df_res_col <- df_res3[order_vals]
# 
# 

















# df1 <- data.frame(New_Argentina = c(2, 4, 6), New_China = c(5, 6, 8), New_Belarus = c(8, 8, 10), Old_Argentina = c(11, 10, 12), 
#                Old_China = c(14, 12, 14), Old_Belarus = c(17, 14, 16))
# 
# df1
# df1[order(gsub(".*_","",colnames(df1)))]
# gsub(".*_","",colnames(df1))
# order(gsub(".*_","",colnames(df1)))
