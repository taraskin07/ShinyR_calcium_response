# Correcting names columns ------------------------------------------------
rename_columns <- function(df, cnames) {
  
  df <- time_col_name(df, name_only = T)
  
  df_output <- df %>%
    rename_with(.cols = -matches("Time"), # selects all columns except "Time"
                ~ paste0(cnames, stringr::str_extract(.x, "\\b\\D*?0*([1-9][0-9]*)", group = 1))
    )
  return(df_output)
}

adding_zeroes <- function(vctr) {
  
  if (stringr::str_length(vctr)==1) {strnum <- paste0('000', vctr)
  } else if (stringr::str_length(vctr)==2) {strnum <- paste0('00', vctr)
  } else if (stringr::str_length(vctr)==3) {strnum <- paste0('0', vctr)
  } else {strnum <- toString(vctr)}
  
  return(strnum)
}

rename_columns_zeros <- function(df, cnames) {
  
  df <- time_col_name(df, name_only = T)
  
  cols = nchar(as.character(max(as.integer(stringr::str_extract(colnames(drt2),
                                                                "\\b\\D*?0*([1-9][0-9]*)")), 
                                na.rm = T)))
  
  df_output <- df %>% 
    rename_with(.cols = -matches("Time"), # selects all columns except "Time"
                ~ paste0(cnames, 
                         unname(sapply(stringr::str_extract(.x, "\\b\\D*?0*([1-9][0-9]*)", group = 1), 
                                       adding_zeroes)) 
                )
    )
  return(df_output)
}


drt2 <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", 
                   sheet = "Ratio")
as.integer(stringr::str_extract(colnames(drt2), "\\b\\D*?0*([1-9][0-9]*)"))
cols = max(as.integer(stringr::str_extract(colnames(drt2),
                                           "\\b\\D*?0*([1-9][0-9]*)")), 
           na.rm = T)
cols = nchar(as.character(max(as.integer(stringr::str_extract(colnames(drt2),
                                                       "\\b\\D*?0*([1-9][0-9]*)")), 
                       na.rm = T)))

typeof(cols)


dfasd <- rename_columns_zeros(dfasd, 'cell-')


str_extract(colnames(dfasd)[2], "\\b\\D*?0*([1-9][0-9]*)", group = 1)


View(dfasd)


all_cells2 <- read_excel("~/Rprojects/Alena/all_cells2.xlsx", 
                         sheet = "ratio")

cols = nchar(as.character(max(as.integer(stringr::str_extract(colnames(all_cells2),
                                                              "\\b\\D*?0*([1-9][0-9]*)")), 
                              na.rm = T)))
cols


max(as.integer(stringr::str_extract(colnames(all_cells2),
                                    "\\b\\D*?0*([1-9][0-9]*)")))




all_cells3 <- rename_columns_zeros(all_cells2, 'cell-')

View(all_cells3)




asd <- paste0(strrep('0', -1), '67')
asd





CleanTable <- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-CleanTable.xlsx", 
                                               sheet = "ratio")
colnames(CleanTable)


rawtable <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", 
                          sheet = "Ratio")


renamed_table <- rename_columns(rawtable, 'cell-')
renamed_table <- rawtable
df_n <- time_col_name(renamed_table, name_only = T)

df <- df_n %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "Signal")

View(df)

wt <- as.numeric(gsub("cell-", "", df$cells))
wt


mywt <- as.numeric(stringr::str_extract(df$cells, '\\b\\D*?0*([1-9][0-9]*)', group = 1))
mywt

mixed <- mixedorder(df$cells, decreasing = TRUE)
mixed


wt2 <- reorder(df$cells, as.numeric(gsub("cell-", "", df$cells)))
wt2


mixed2 <- reorder(df$cells, mixed)
mixed2


mywt2 <- reorder(df$cells, mywt)
mywt2


reorder(df$cells, mywt)['scores']
reorder(df$cells, mixed)

factor(colnames(renamed_table)[-1], colnames(renamed_table)[-1])
















CleanTable <- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-CleanTable.xlsx", 
                         sheet = "ratio")




testTable <- read_excel("~/Rprojects/Alena/all_cells2.xlsx", 
                        sheet = "ratio")



nchar(as.character(max(as.numeric(na.omit(stringr::str_extract(colnames(testTable),
                     "\\b\\D*?0*([1-9][0-9]*)", group = 1))))))









