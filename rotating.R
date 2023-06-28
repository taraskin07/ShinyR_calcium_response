

dfrot <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002-CleanTable.xlsx", sheet = 'ratio')
dfrot <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", sheet = 'Ratio')
dfrot <- read_excel("~/Rprojects/Test_files/2023-04-29-mpkCCD007-Shifted_with_Average.xlsx", sheet = "Ratio_shifted")


dfrot 


df123 <- dfrot %>% 
  pivot_longer(!Time, names_to = "cells", values_to = "Signal") 



length(unique(df123$cells))

nlevels(df123$cells)




ggplotly_render(dfrot, ready=F) + theme(legend.position = 'none')

ggplotly_render(dfrot, ready=T)



df_to_render <- time_col_name(dfrot)


cell_name <- finding_cell_name(dfrot, 27)

cell_name


render_plot <- df_to_render %>% 
  select('Time', all_of(cell_name))






























dfrot <- dfrot %>% 
  add_column(Average = rowMeans(dfrot[-grep('^Time$', colnames(dfrot))]))





grep("^([Aa]verage|[Mm]ean)", colnames(dfrot))

c(grep('^Time$', colnames(dfrot)), grep("^([Aa]verage|[Mm]ean)", colnames(dfrot)))

dfrot <- dfrot[, c(grep('^Time$', colnames(dfrot)), grep("^([Aa]verage|[Mm]ean)", colnames(dfrot)))]



average_curve <- function(df_read) {
  
  df_rot <- time_col_name(df_read)
  
  if (length(grep("^([Aa]verage|[Mm]ean)", colnames(dfrot))) == 0) {
    
    df_rot <- df_rot %>% 
      add_column(Average = rowMeans(df_rot[-grep('^Time$', colnames(df_rot))]))
      
    
    df_rot <- df_rot[, c(grep('^Time$', colnames(df_rot)), grep("^([Aa]verage|[Mm]ean)", colnames(df_rot)))]

  } else {
    
    df_rot <- df_rot %>% 
      select(c(grep('^Time$', colnames(df_rot)), grep("^([Aa]verage|[Mm]ean)", colnames(dfrot))))
  }
  
  return(df_rot)
  
}




rotating_curve <- function(df_to_rotate, lower_t, upper_t, shift_down = 0) {
  
  
  initial_col_name <- colnames(df_to_rotate)[grep("^([Aa]verage|[Mm]ean)", colnames(df_to_rotate))]
  
  colnames(df_to_rotate)[grep("^([Aa]verage|[Mm]ean)", colnames(df_to_rotate))] <- 'Average'
  
  
  
  df_1 <- subset(df_to_rotate, Time < lower_t) 
  
  df_2 <- subset(df_to_rotate, (Time >= lower_t & Time <= upper_t))
  
  df_3 <- subset(df_to_rotate, Time > upper_t) 
  

  
  # Rotating
  
  average_lm <- coef(lm(Average ~ Time, data = df_2))
  
  b = average_lm[[1]]
  k = average_lm[[2]]
  
  
  df_2$Average <- df_2$Average-k*df_2$Time
  
  if (shift_down %% 2 == 1) {
    
    df_2$Average <- df_2$Average + k*df_2$Time[length(df_2$Time)]
    
  }
  
  
  df_out <- rbind(df_1, df_2, df_3)
  colnames(df_out)[which(names(df_out) == 'Average')] <- initial_col_name
  
  return(df_out)
}







seeee <- rotating_curve(cleaned_df, 15, 130)

cleaned_df <- average_curve(dfrot)

initial_col_name <- colnames(cleaned_df)[grep("^([Aa]verage|[Mm]ean)", colnames(cleaned_df))]
initial_col_name

colnames(cleaned_df)[grep("^([Aa]verage|[Mm]ean)", colnames(cleaned_df))] <- 'Mean'
cleaned_df

colnames(cleaned_df)[which(names(cleaned_df) == 'Mean')] <- initial_col_name
cleaned_df


subsetted <- subset(cleaned_df, (Time < 135))
# View(subsetted)





# ggplotly_render(subsetted)





average_lm <- lm(Average ~ Time, data = subsetted)





# View(average_lm)

cf <- coef(average_lm)
cf
b = cf[[1]]
b

k = cf[[2]]
k

eq <-paste0('y = ', k, 'x+', b) 
eq

subsetted$y <- subsetted$Time*k + b
subsetted




subsetted$extract <- as.double(str_extract(subsetted$Average, '\\d\\.(\\d{3})\\d+', group = 1))



subsetted$slope <- subsetted$Time*(-0.4) + subsetted$extract
subsetted





# Testing function rotating_curve

test_df <- select(subsetted, c('Time', 'slope'))

names(test_df) <- c('Time', 'Average')

test_df

plot(test_df$Time, test_df$Average, asp = 1, xlab = "Time",
     ylab = 'Mean', yaxp = c(60, 140, 4))


rot_test_df <- rotating_curve(test_df, 20, 100, 1)


plot(rot_test_df$Time, rot_test_df$Average, asp = 1, xlab = "Time",
     ylab = 'Mean', yaxp = c(60, 140, 4))










slope_lm <- lm(slope ~ Time, data = subsetted)

plot(subsetted$Time, subsetted$slope, asp = 1, xlab = "Time",
     ylab = 'slope', yaxp = c(60, 140, 4))





cf2 <- coef(slope_lm)
cf2
b2 = cf2[[1]]
b2

k2 = cf2[[2]]
k2

eq <-paste0('y = ', k2, 'x', '+',b2) 
eq





# par(mar = c(1, 1, 1, 1))
plot(subsetted$Time, subsetted$slope, asp = 1, xlab = "Time",
     ylab = 'slope', yaxp = c(60, 140, 4))
abline(slope_lm)



subsetted$correct <- subsetted$slope-k2*subsetted$Time

# par(mar = c(1, 1, 1, 1))
plot(subsetted$Time, subsetted$correct, asp = 1, xlab = "Time",
     ylab = 'correct', yaxp = c(60, 140, 4))
abline(lm(correct ~ Time, data = subsetted))
subsetted

subsetted$Time[length(subsetted$Time)]
subsetted$shifted <- subsetted$correct - k2*subsetted$Time[length(subsetted$Time)]
plot(subsetted$Time, subsetted$shifted, asp = 1, xlab = "Time",
     ylab = 'shifted', yaxp = c(60, 140, 4))
















































ggplotly_render(cleaned_df)





cleaned_df <- average_curve(dfrot)
View(cleaned_df)



colnames(dfrot)
str_extract(colnames(dfrot), 'Time')

grep('Time', dfrot)

if (length(grep("Time \\[s\\]", colnames(dfrot))) == 0) {print('Yes')} else {print('No')}

str_view(
  fstr,
  pattern = 'Time',
  match = TRUE,
  html = FALSE,
  use_escapes = FALSE
)

grep('Time [s]', colnames(dfrot))


str_extract(colnames(dfrot), '[tT]ime.*')



fstr <- 'qwertyuioTimeasdfghjhj'
grepl('Time', fstr)


str_locate(fstr, 'Time')
str_locate(fstr, 'Time')[1]
str_locate(fstr, 'Time')[2]
substr(fstr, str_locate(fstr, 'Time')[1], str_locate(fstr, 'Time')[2])




switchInput(
  inputId,
  label = NULL,
  value = FALSE,
  onLabel = "ON",
  offLabel = "OFF",
  onStatus = NULL,
  offStatus = NULL,
  size = "default",
  labelWidth = "auto",
  handleWidth = "auto",
  disabled = FALSE,
  inline = FALSE,
  width = NULL
)
























dfrot <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003SOCE.xlsx", 
                                        sheet = "Ratio")
dfrot <- read_excel("~/Rprojects/Test_files/2023-05-12-mpkCCD003-CleanTable-Shifted.xlsx", 
                                                       sheet = "ratio")
cell <- 53


getting_a_slice_of_df <- function(df_to_slice, cell_number) {
  
  df_to_slice <- time_col_name(df_to_slice)
  cell_name <- finding_cell_name(df_to_slice, cell_number)
  
  df_to_slice <- df_to_slice %>%
    select('Time', all_of(cell_name))
  
  return(df_to_slice)

  }


df_to_see <- getting_a_slice_of_df(dfrot, cell)



df_to_see_out <- rotating_plot(df_to_see, 200, 500)


ggplotly_render(df_to_see)
ggplotly_render(df_to_see_out)


# TESTS


#1
df_to_see_test <- df_to_see[, c(2,1)]

colnames(df_to_see_test)[1] != 'Time'
grep('([Tt]ime\\s?)', colnames(df_to_see_test))


df_to_see_out_test <- rotating_plot(df_to_see_test, 200, 500)
ggplotly_render(df_to_see_out_test)

#2


chnam <- "Time [s]"
chnam
colnames(df_to_see_test)[2] <- "Time [s]"
df_to_see_test
df_to_see_out_test <- rotating_plot(df_to_see_test, 200, 500)
ggplotly_render(df_to_see_out_test)


#3

df_to_see_test2 <- df_to_see

colnames(df_to_see_test2)[1] <- "Time [s]"
df_to_see_test2
df_to_see_out_test2 <- rotating_plot(df_to_see_test2, 200, 500)
ggplotly_render(df_to_see_out_test2)


#4

df_to_see_test3 <- df_to_see

colnames(df_to_see_test3)[1] <- "Timrgnrfgnr"
df_to_see_test3
df_to_see_out_test3 <- rotating_plot(df_to_see_test3, 200, 500)
ggplotly_render(df_to_see_out_test3)



#4

df_to_see_test4 <- df_to_see[1]


df_to_see_test4
df_to_see_out_test4 <- rotating_plot(df_to_see_test4, 200, 500)
ggplotly_render(df_to_see_out_test4)

dfrotav <- average_curve(dfrot)
asd <- rotating_curve(dfrotav, 0, 120)
asd2 <- rotating_curve(dfrotav, 0, 120, shift_down = T)
asd3 <- rotating_plot(dfrotav, 0, 120, part = T)
asd4 <- rotating_plot(dfrotav, 0, 120, part = T, shift_down = T)

ggplotly_render(dfrotav)
ggplotly_render(asd)
ggplotly_render(asd2)
ggplotly_render(asd3)
ggplotly_render(asd4)


rotating_plot <- function(df_to_rotate, lower_t, upper_t, part = FALSE, shift_down = FALSE) {
  
  
  if (ncol(df_to_rotate) != 2) {stop(print("Something wrong with the data: no such cell number or they are repeats!"))
    
  } else if (colnames(df_to_rotate)[1] != 'Time') {
    
    if (length(grep('([Tt]ime\\s?)', colnames(df_to_rotate))) != 1) {
      
      stop(print("Something wrong with the data: no Time column!"))
      
    } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 1) {
      
      colnames(df_to_rotate)[1] <- 'Time'
      
      
    } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 2) {
      
      colnames(df_to_rotate)[2] <- 'Time'
      df_to_rotate <- df_to_rotate[, c(2,1)]
    
      } else {
      
      stop(print("Something wrong with the data: no Time column!"))
      
             }


  } 
      
    
    
  initial_col_name <- colnames(df_to_rotate)[2]
  
  colnames(df_to_rotate)[2] <- 'Cell'
  
  
  
  df_1 <- subset(df_to_rotate, Time < lower_t) 
  
  df_2 <- subset(df_to_rotate, (Time >= lower_t & Time <= upper_t))
  
  df_3 <- subset(df_to_rotate, Time > upper_t) 
  
  
  # Rotating
  
  average_lm <- coef(lm(Cell ~ Time, data = df_2))
  
  # b = average_lm[[1]]
  k = average_lm[[2]]
  
  
  
  # Rotate partially or the whole plot?
  
  if (part == T) {
    
    df_2$Cell <- df_2$Cell-k*df_2$Time
    
    if (shift_down == TRUE) {
      
      df_2$Cell <- df_2$Cell + k*df_2$Time[length(df_2$Time)]
      
    }
    
    
    df_to_rotate <- rbind(df_1, df_2, df_3)
    
  } else {df_to_rotate$Cell <- df_to_rotate$Cell-k*df_to_rotate$Time}
  
  

  
  # Returning initial column name if differs
  
  colnames(df_to_rotate)[which(names(df_to_rotate) == 'Cell')] <- initial_col_name
  
  return(df_to_rotate)
  
  
}






