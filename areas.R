


rotated_df <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003-CleanTable-Shifted-Rotated.xlsx", 
                                                               sheet = "data_rotated")
ncol(rotated_df)





area_start <- 560
area_end <- 775
baseline_start <- 0
baseline_end <- 120

idx_c <- c()
res_table <- data.frame(Cell = character(), 
                        Area = numeric(), 
                        Difference = numeric(),
                        Area_by_Dif = numeric(),
                        Maximum = numeric(),
                        Baseline_average = numeric(),
                        SD_baseline = numeric(),
                        CV_baseline_pct = numeric()
                        )



for (idx in 2:ncol(rotated_df)) {
  
  df_slice <- rotated_df[, c(1, idx)]
  b <- b_find(df_slice, baseline_start, baseline_end)
  intersections_df <- dataframe_with_intersections(df_slice, area_start, area_end, b)
  dfres <- polygon_function(intersections_df, area_start, area_end, b)
  dfres[, 1] <- intersections_df[, 1]
  
  auc_upper <-  AUC(dfres[[1]], 
                    dfres[[2]], 
                    from = area_start, 
                    to = area_end, 
                    method = "trapezoid")
  
  auc_lower <- AUC(dfres[[1]], 
                   rep(b, length.out = nrow(dfres)), 
                   from = area_start, 
                   to = area_end, 
                   method = "trapezoid")
  
  auc <- auc_upper - auc_lower
  
  sd = SD(subset(df_slice, (Time >= baseline_start) & (Time <= baseline_end))[[2]])
  cv = (sd / b)*100
  maximum = max(subset(df_slice, (Time >= area_start) & (Time <= area_end))[[2]])
  difference <- maximum - b
  
  res_table[nrow(res_table)+1, ] <-
    c(Cell = colnames(rotated_df)[idx],
      Area = decim(auc),
      Difference = decim(difference),
      Area_by_Dif = decim(auc/difference),
      Maximum = decim(maximum),
      Baseline_average = decim(b),
      SD_baseline = decim(sd),
      CV_baseline_pct = decim(cv, 1)
      )

  
  # if (auc < 20) {
  #   idx_c <- c(idx_c, idx)
  #   print(paste0('For ', colnames(rotated_df)[idx], ' and id ', idx, ', auc is: ', auc))
  # }
}

res_table



for (n in 2:ncol(result_table)) {
  res_table[, n] <- as.numeric(res_table[, n])
}


res_statistics <- data.frame(
  Mean_Area = decim(mean(res_table$Area)),
  SD_Area = decim(SD(res_table$Area)),
  CV_Area_pct = decim(100*SD(res_table$Area)/mean(res_table$Area), 1),
  Mean_Difference = decim(mean(res_table$Difference)),
  SD_Difference = decim(SD(res_table$Difference)),
  CV_Difference_pct = decim(100*SD(res_table$Difference)/mean(res_table$Difference), 1),
  Shapiro_Wilk_area_W = decim(shapiro.test(res_table$Area)[[1]]),
  Shapiro_Wilk_area_P = shapiro.test(res_table$Area)[[2]],
  Shapiro_Wilk_difference_W = decim(shapiro.test(res_table$Difference)[[1]]),
  Shapiro_Wilk_difference_P = shapiro.test(res_table$Difference)[[2]],
  Mean_A_by_d = decim(mean(res_table$Area_by_Dif)),
  CV_A_by_d_pct = decim(100*SD(res_table$Area_by_Dif)/mean(res_table$Area_by_Dif), 1),
  Mean_Baseline_av = decim(mean(res_table$Baseline_average)),
  CV_Baseline_av_pct = decim(100*SD(res_table$Baseline_average)/mean(res_table$Baseline_average), 1)
)

res_statistics

































res_table[which(res_table$Cell == colnames(rotated_df)[5]), ] <-c(Cell = 'colnames(rotated_df)[idx]',
                                                                           Area = 'decim(auc)',
                                                                           Difference = 'decim(difference)',
                                                                           Area_by_Dif = 'decim(auc/difference)',
                                                                           Maximum = 'decim(maximum)',
                                                                           Baseline_average = 'decim(b)',
                                                                           SD_baseline = 'decim(sd)',
                                                                           CV_baseline_pct = 'decim(cv, 1)'
)

df_slice <- rotated_df[, c(1, 5)]
intersections_df <- dataframe_with_intersections(df_slice, area_start, area_end, b)
dfres <- polygon_function(intersections_df, area_start, area_end, b)
dfres[, 1] <- intersections_df[, 1]

auc_upper <-  AUC(dfres[[1]], 
             dfres[[2]], 
             from = area_start, 
             to = area_end, 
             method = "trapezoid")


auc_lower <- AUC(dfres[[1]], 
                 rep(b, length.out = nrow(dfres)), 
                 from = area_start, 
                 to = area_end, 
                 method = "trapezoid")
auc <- auc_upper - auc_lower
