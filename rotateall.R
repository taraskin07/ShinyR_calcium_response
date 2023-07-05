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


mpkCCD003SOCE_Rotated <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003SOCE-Rotated.xlsx", 
                                    sheet = "data_rotated")



dcm <- Vectorize(decim)

dcm(mpkCCD003SOCE_Rotated[-grep('([Tt]ime\\s?)', colnames(mpkCCD003SOCE_Rotated))])





mpkCCD003SOCE_Rotated2 <- cbind(mpkCCD003SOCE_Rotated[grep('([Tt]ime\\s?)', colnames(mpkCCD003SOCE_Rotated))], 
                              dcm(mpkCCD003SOCE_Rotated[-grep('([Tt]ime\\s?)', colnames(mpkCCD003SOCE_Rotated))]), 3)
asadasda <- average_curve(mpkCCD003SOCE_Rotated)

ggplotly_render(asadasda)

# --------------------------------------------------------------------------------------
mpkCCD003SOCE_Rotated <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003SOCE-Rotated.xlsx", 
                                    sheet = "data_rotated")

data_frame_input <- mpkCCD003SOCE_Rotated
data_frame_input <- time_col_name(data_frame_input)
b_max <- 120
b_min <- 0


subsetted <- subset(data_frame_input, (Time <= b_max & Time >= b_min))
subsetted[25]
b <- mean(subsetted[[25]])
b


pl <- data_frame_input[c(1,25)]


ggplotly_render(pl,
                baseline = T,
                b_min = 0,
                b_max = 120,
                region = T,
                r_min = 560,
                r_max = 760,
                ready = FALSE) +
  scale_color_manual(values='black') +
  geom_hline(yintercept=b, color = "blue") +
  polygon_function(pl, 560, 760, 25, b) + theme(legend.position = "none")





df_rotated <- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-TEST.xlsx", 
                                                  sheet = "ratio")

df_rotated <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003-CleanTable-Shifted-Rotated.xlsx", 
                                                               sheet = "data_rotated")

ggplotly_render(average_curve(df_rotated))

# df_rotated <- getting_a_slice_of_df(df_rotated, 40)

df_rotated <- average_curve(df_rotated)


find_all_intersections <-  function(timeframe, b, r_min, r_max) {
  
  timeframe <- time_col_name(timeframe)
  init_name <- colnames(timeframe)[2]
  colnames(timeframe)[2] <- 'Current'

  
  df_part <-subset(timeframe, (Time <= r_max & Time >= r_min))
  
  
  for (value in df_part$values) {
    
    
    
  }
  
} 


timeframe <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003-CleanTable-Shifted-Rotated.xlsx", 
                         sheet = "data_rotated")

timeframe <- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-TEST.xlsx", 
           sheet = "ratio")


timeframe <- average_curve(timeframe)

# r_min <- 500
# r_max <- 755

r_min <- 120
r_max <- 330

averb <- b_find(timeframe, 0, 120)
b <- averb

plot_func(timeframe)



polygon_df <- dataframe_with_intersections(timeframe, r_min, r_max, b)







df_rotated2 <- polygon_df

df_rotated2$Current[(df_rotated2$Time > (r_min - region1) & df_rotated2$Time < (r_max + region2) & df_rotated2$Current < averb)] <- averb
df_rotated2$Current[(df_rotated2$Time <= (r_min - region1) | df_rotated2$Time > (r_max + region2))] <- averb
df_rotated2$Time[df_rotated2$Time > (r_max + region2)] <- r_max

df_rotated2 <- polygon_function(polygon_df, r_min, r_max, b) 

df_rotated2

plot <-  ggplotly_render(polygon_df,baseline = T,
                         b_min = 0,
                         b_max = 120,
                         region = T,
                         r_min = r_min,
                         r_max = r_max, ready = F) +
  theme(legend.position = "none") + 
  scale_color_manual(values='black') + 
  geom_hline(yintercept=averb, color = "blue") + 
    geom_polygon(mapping=aes(x=df_rotated2$Time, y=df_rotated2$Current), fill = 'green')+
  geom_point(mapping=aes(df_rotated2$Time, df_rotated2$Current), size = 1, colour = "red")
ggplotly(plot)


# > square_auc
# [1] 48.21984


df_rotated2






auc2 <-  AUC(polygon_df$Time, 
             df_rotated2$Current, 
             from = r_min, 
             to = r_max, 
             method = "trapezoid")
auc2
# [1] 300.0014

auc1 <- AUC(polygon_df$Time, 
            rep(b, length.out = length(polygon_df$Time)), 
            from = r_min, 
            to = r_max, 
            method = "trapezoid")
auc1
# [1] 251.7816
square_auc <- auc2 - auc1
square_auc
# [1] 48.21984

auc2_1 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 121, 
              to = 134, 
              method = "trapezoid")
auc2_1

auc2_2 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 146, 
              to = 221, 
              method = "trapezoid")
auc2_2


auc2_3 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 251, 
              to = 330, 
              method = "trapezoid")
auc2_3


auc2_1+auc2_2+auc2_3
# [1] 248.4462



auc1_1 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 121, 
              to = 134, 
              method = "trapezoid")
auc1_1

auc1_2 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 146, 
              to = 221, 
              method = "trapezoid")
auc1_2


auc1_3 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 251, 
              to = 330, 
              method = "trapezoid")
auc1_3

auc1_1+auc1_2+auc1_3
# [1] 200.2263

auc2_1+auc2_2+auc2_3 - (auc1_1+auc1_2+auc1_3)
# [1] 48.21984
square_auc



# SPLINES

auc2 <-  AUC(polygon_df$Time, 
             df_rotated2$Current, 
             from = r_min, 
             to = r_max, 
             method = "spline")
auc2
# [1] 300.0014

auc1 <- AUC(polygon_df$Time, 
            rep(b, length.out = length(polygon_df$Time)), 
            from = r_min, 
            to = r_max, 
            method = "spline")
auc1
# [1] 251.7816
square_auc <- auc2 - auc1
square_auc
# [1] 48.21984

auc2_1 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 121, 
              to = 134, 
              method = "spline")
auc2_1

auc2_2 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 146, 
              to = 221, 
              method = "spline")
auc2_2


auc2_3 <- AUC(polygon_df$Time, 
              polygon_df$Current, 
              from = 251, 
              to = 330, 
              method = "spline")
auc2_3


auc2_1+auc2_2+auc2_3
# [1] 248.4462



auc1_1 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 121, 
              to = 134, 
              method = "spline")
auc1_1

auc1_2 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 146, 
              to = 221, 
              method = "spline")
auc1_2


auc1_3 <- AUC(polygon_df$Time, 
              rep(b, length.out = length(polygon_df$Time)), 
              from = 251, 
              to = 330, 
              method = "spline")
auc1_3

auc1_1+auc1_2+auc1_3
# [1] 200.2263

auc2_1+auc2_2+auc2_3 - (auc1_1+auc1_2+auc1_3)
# [1] 48.21984
square_auc
