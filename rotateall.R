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





x <- find_intersection(data_frame_input, 560, 25, b)
x

area25 <- subset(NA_Rotated, (Time < (r_min + 25) & Time > (r_min - 25)))

area25[area25$Average > averb, ]['Average']


y <- averb
y
y2 <-  min(area25[area25$Average > averb, ]['Average'])
y2
y1 <-  max(area25[area25$Average <= averb, ]['Average'])
y1
x1 <- area25[area25$Average == y1, ][["Time"]]
x1
x2 <- area25[area25$Average == y2, ][["Time"]]
x2


x <- x1+(y-y1)*(x2-x1)/(y2-y1)
x

ggplotly(ccurve2)


subset_poly <- subset(NA_Rotated, (Time <= r_max & Time >= r_min))
polygonX <- subset_poly$Time
polygonY <- subset_poly$Average

polygonX
length(polygonX)
polygonY
length(polygonY)

nrow(NA_Rotated)

polygonX <- c(polygonX, r_max, r_min)
polygonY <- c(polygonY, y, y)

polygonX <- c(polygonX, rep(x, length.out=(nrow(NA_Rotated)-length(polygonX))))
polygonY <- c(polygonY, rep(y, length.out=(nrow(NA_Rotated)-length(polygonY))))


polygonX
length(polygonX)
polygonY
length(polygonY)

data_poly <- data.frame(X=polygonX, Y=polygonY)

polygonX
length(polygonX)
polygonY
length(polygonY)

polyX <- rep(polygonX, length.out=nrow(NA_Rotated))
length(polyX)


ccurve3 <- ccurve2 +
  geom_polygon(mapping=aes(x=polygonX, y=polygonY), fill = 'green')

ggplotly(ccurve3)













# AUC(x, y, from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), 
#     method = c("trapezoid", "step", "spline", "linear"), 
#     absolutearea = FALSE, subdivisions = 100, na.rm = FALSE, ...) 

auc2 <-  AUC(NA_Rotated$Time, NA_Rotated$Average, from = r_min, to = r_max, method = "spline")
auc2


auc1 <- AUC(NA_Rotated$Time, rep(y, length.out = length(NA_Rotated$Time)), from = r_min, to = r_max, method = "spline")
auc1

square_auc <- auc2 - auc1
square_auc















