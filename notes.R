

library(readxl)


rename_columns <- function(df) {
  
  df_output <- df %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0("cell-", str_remove(stringr::str_split_i(.x, " ", 1), "#") 
                )
    )
  return(df_output)
}

library(writexl)

nl <- list('340'=df1, '380'=df2)

write_xlsx(
  nl,
  path = 'TempExcel.xlsx',
  col_names = TRUE,
  format_headers = TRUE,
)

time_col_name <- function(datafr) {
  if (colnames(datafr)[1] == "Time [s]") {
    colnames(datafr)[1] =  "Time"
  }
  datafr$Time <- round(datafr$Time)
  return(datafr)
}


path <-"files/2022-06-09-mpkCCD001-RawData(SOCE).xlsx"
df_output1 <- read_excel(path, sheet='340')
df_output2 <- read_excel(path, sheet='380')


df1 <- rename_columns(df_output1, 'cell-'); df1
df2 <- rename_columns(df_output1, 'cell-'); df2
df1 <- time_col_name(df1)
df2 <- time_col_name(df2)
nam <- colnames(df1[1])
nam

library(pastecs)
basic_statistics <- function(df) {
  df <- df %>% 
    distinct(across(-1))

  res <- stat.desc(df)
  res_t <- as.data.frame(t(as.matrix(res)))
  options(scipen=10)
  options(digits=3)
  return(res_t)
  
}


stat_340 <- basic_statistics(df1)
library(ggplot2)
library(plotly)
# p <- ggplot(df1, aes(Time, r.u., group = cells, color = cells)) + geom_line(size=0.5)+
#   geom_point(size = 0.2) + theme(legend.position = "none")
# 
# ggplotly(p)

df <- df1[, 1:4]; df

df_tidy_340 <- df1 %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "r.u.")

df_tidy_340

p <- ggplot(df_tidy_340, aes(Time, r.u., group = cells, color = cells)) + geom_line(linewidth=0.5)+ geom_point(size = 0.2) + theme(legend.position = "none")
ggplotly(p)






library(readxl)
dataframe_to_check <- read_excel("test/CleanTable.xlsx", sheet = "340")

clnms <-colnames(dataframe_to_check)
print(clnms)

clist <- list("cell-1", "cell-20", "cell-3")
print(clist)

what <- subset(dataframe_to_check, select = !(colnames(dataframe_to_check) %in% clist))

print(what)



typeof(what)

mb <- basic_statistics(what)


# mb$cell_names <- rownames(mb) 
# mb <- mb[, c(ncol(mb), 1:(ncol(mb)-1))] # Make row_names first column
mb <- cell_number_row(mb)
write_xlsx(list('340'=mb, '380' = mb), path = 'new_file.xlsx')




typeof(mb)




library(xts)



# Amplitude ---------------------------------------------------------------








gr <- df %>%
  group_by(cell) %>% 
  filter(df, Time < 200) %>% 
  (~ sub_range) %>% 
  # summarise(
  #   Max = max(value, na.rm = T),
  #   TimeMax = paste(Time[which(value == Max)], collapse = ", "),
  #   Min = min(sub_range$value, na.rm = T),
  #   TimeMin = paste(Time[which(value == Min)], collapse = ", "),
  #   # Amplitude = diff(pull(Max), pull(Min))
  #   Filter = filter(df, Time < 200),
  # 
  #   
  #   ) %>%
  # arrange(cell)
  # print(sub_range)

# library('gtools')

library(readxl)
cl_df <- read_excel("test/CleanTable.xlsx", sheet = "ratio")


df <- cl_df %>% 
  pivot_longer(!Time, names_to = "cell", values_to = "value")

View(df)

subset_timerange <- subset(df, (Time < 200 & Time > 5))

View(subset_timerange)

cell_index <- colnames(cl_df[-1])
cell_index

timerange_grouped <- group_by(subset_timerange, cell)

result_max <- summarize(group_by(df, cell),
  Max = max(value, na.rm = T),
  TimeMax = paste(Time[which(value == Max)], collapse = ", "))

result_min <- summarize(group_by(subset_timerange, cell),
                        Min = min(value, na.rm = T),
                        TimeMin = paste(Time[which(value == Min)], collapse = ", "))


result_amplitude <- merge(result_max, result_min, by = "cell")
result_amplitude_final <- result_amplitude %>% 
  add_column(Amplitude = result_amplitude$Max - result_amplitude$Min) %>% 
  arrange(factor(cell, cell_index))
  


gr <- df %>%
  group_by(cell) %>% 
  summarize(
    Max = max(value, na.rm = T),
    TimeMax = paste(Time[which(value == Max)], collapse = ", "),
    
    ) %>% 

    arrange(factor(cell, cell_index))

View(gr)






# Testing find_amplitude --------------------------------------------------

clean_df <- read_excel("test/CleanTable.xlsx", sheet = "ratio")
clean_df <- read_excel("test/CleanTable.xlsx", sheet = "340")
clean_df_380 <- read_excel("test/CleanTable.xlsx", sheet = "380")

amplitude <- find_amplitude(clean_df, 0, 120)
amplitude <- find_amplitude_380(clean_df_380, 0, 120)

ampl_calculating <- amplitude %>% 
  summarize(
    Amplitude_average = decim(mean(Amplitude, na.rm = T), 3),
    Amplitude_SD = decim(sd(Amplitude, na.rm = T), 3),
    Amplitude_3SD = decim(3*Amplitude_SD, 3),
    Amplitude_SE_percent = decim(100*Amplitude_SD/Amplitude_average, 1),
    Average_Baseline_SE = decim(mean(Baseline_SE, na.rm = T), 1),
    Amount_of_cells = nrow(amplitude)
  )


colnames(amplitude)


asdgvaegva <- summarize_amplitudes(amplitude)



# ROUNDING VALUES


decim <- function(number, digits) {
  
  
  round_result <- as.numeric(format(round(number, digits), nsmall = digits))
  
  return(round_result)
}


x <- 4.3457
x <- '4.345'
x <- 4.345
x <- 41345134651.345
x <- 4.345

typeof(decim3(x))
decim(x, 3)




# name constructor --------------------------------------------------------

fn <- "ProcessedTable.xlsx"

filename(fn,fn)

strsplit('ProcessedTable.xlsx', split = "[.]")



# Correcting numbers ------------------------------------------------------

dfr <- read_excel("files/2022-02-25-mpkCCD002-Raw(UTP).xlsx", sheet = "Ratio")

dfr_r <- rename_columns(dfr, 'something-')


adding_zeroes <- function(vctr) {
  
  if (stringr::str_length(vctr)==1) {strnum <- paste0('00', vctr)
  } else if (stringr::str_length(vctr)==2) {strnum <- paste0('0', vctr)
  } else {strnum <- toString(vctr)}
  
  return(strnum)
}

adding_zeroes_vect <- Vectorize(adding_zeroes)

rename_columns_zeros <- function(df, cnames) {
  
  df_output <- df %>% 
    rename_with(.cols = contains("#"), # selects only the data columns
                ~ paste0(cnames, unname(sapply(str_remove(stringr::str_split_i(.x, " ", 1), "#"), adding_zeroes)) 
                )
    )
  return(df_output)
}

str_remove(stringr::str_split_i(colnames(dfr)[-1], " ", 1), "#")
sdfads <- adding_zeroes_vect(str_remove(stringr::str_split_i(colnames(dfr)[-1], " ", 1), "#"))
sdfads[1]

typeof(sdfads[1])
strres <- adding_zeroes_vect(c('1', '20', '456'))

adding_zeroes_vect(strres)

unname(sapply(strres, adding_zeroes))

adsasdasd <- rename_columns_zeros(dfr,'something-')

paste0('something-', strres)
