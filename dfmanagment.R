dfrot <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003SOCE.xlsx", 
                    sheet = "Ratio")
cell <- 1

dfrot <- time_col_name(dfrot)


df_to_see <- getting_a_slice_of_df(dfrot, cell)



df_to_see_out <- rotating_plot(df_to_see, 200, 500)
colnames(df_to_see_out)

!grepl('([Tt]ime\\s?)', colnames(df_to_see_out))



inter <- intersect(colnames(dfrot), colnames(df_to_see_out)[!grepl('([Tt]ime\\s?)', colnames(df_to_see_out))])
inter


which(colnames(dfrot)==inter)

which

df_to_see_out[!grepl('([Tt]ime\\s?)', colnames(df_to_see_out))]

dfrot[53] <- df_to_see_out[!grepl('([Tt]ime\\s?)', colnames(df_to_see_out))]

dfrot

colnames(dfrot)[colnames(dfrot) == intersect(colnames(dfrot), colnames(df_to_see_out))]


replace_columns_in_dfs <- function(df_full, df_part) {
  
  inter <- intersect(colnames(df_full), colnames(df_part)[!grepl('([Tt]ime\\s?)', colnames(df_part))])
  
  
  df_full[which(colnames(df_full)==inter)] <- df_part[!grepl('([Tt]ime\\s?)', colnames(df_part))]
  
  return(df_full)
  
}



asdasdasdasd <- replace_columns_in_dfs(dfrot, df_to_see_out)








ggplotly_render(df_to_see)
ggplotly_render(df_to_see_out)
