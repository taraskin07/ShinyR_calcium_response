mpkCCD003SOCE <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003SOCE.xlsx", 
                                        sheet = "Ratio")


outersect(list_of_names, (colnames(mpkCCD003SOCE)[-grep('([Tt]ime\\s?)', colnames(mpkCCD003SOCE))]))

ncol(mpkCCD003SOCE)

rotate_all <- function(df_to_rotate, list_of_names, listn = FALSE, lower_base, upper_base, lower_reg, upper_reg, baseline_r = TRUE, shift_down = TRUE) 
  
  
  
  {

  if (listn == FALSE) {names_list <- colnames(df_to_rotate)[-grep('([Tt]ime\\s?)', colnames(df_to_rotate))]
  
  } else {
  
  names_list <- outersect(list_of_names, (colnames(df_to_rotate)[-grep('([Tt]ime\\s?)', colnames(df_to_rotate))]))

  }
  
  
  for (name in names_list)
  {
    
    df2dim_single_and_rotated <- rotating_plot(getting_a_slice_of_df(df_to_rotate, name, c_name = T), lower_reg, upper_reg, part = FALSE)
    
    if (baseline_r == TRUE) {
    
    df2dim_single_and_rotated_part <- rotating_plot(df2dim_single_and_rotated, lower_base, upper_base, part = TRUE, shift_down)
    
    df_to_rotate <- replace_columns_in_dfs(df_to_rotate, df2dim_single_and_rotated_part)
    
  } else {
      
    df_to_rotate <- replace_columns_in_dfs(df_to_rotate, df2dim_single_and_rotated)
    
  }
}
  return(df_to_rotate)
  
  

}





list_of_names <- colnames(mpkCCD003SOCE)[c(2,3,5,10)]
list_of_names


new <- rotate_all(mpkCCD003SOCE, list_of_names, listn = T, lower_base = 0, upper_base = 120, lower_reg = 200, upper_reg = 500, baseline_r = TRUE, shift_down = TRUE)



View(new)
