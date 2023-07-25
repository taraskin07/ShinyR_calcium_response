
source("../../engine.R")




test_that("list_to_save", {
  

  expect_equal(list_to_save(c(1, 2, 3), c('one', 'two', 'three'), c(T, F, T)), c('one' = 1, 'three' = 3))
  
  
  expect_error(list_to_save(c(1, 3), c('one', 'two', 'three'), c(T, F, T)), "List's length are not equal!", fixed=TRUE)
  expect_error(list_to_save(c(1, 2, 3), c('one', 'three'), c(T, F, T)), "List's length are not equal!", fixed=TRUE)
  expect_error(list_to_save(c(1, 2, 3), c('one', 'two', 'three'), c(T, F)), "List's length are not equal!", fixed=TRUE)

})




c(T,T, F,(T & F))



d340 <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", 
                                    sheet = "340")

d380 <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", 
                                    sheet = "380")

drt2 <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", 
                                    sheet = "Ratio")
length(drt2)


named_list <- setNames(random_color_generator(drt2), colnames(drt2)[-1])
named_list


colors2000 <- randomColor(count = 2000, hue = 'random', luminosity = 'bright')


pattern <- '\\b\\D*?0*([1-9][0-9]*)'

current_palette <- character()

for (idx in 2:ncol(drt2)) {

  color_id <- str_extract(colnames(drt2)[idx], pattern, group = 1)

  if (idx > 2000) {
    color_id <- sample.int(1999, 1)
  }
  
  current_palette <-c(current_palette, colors2000[[as.integer(color_id)]])
  
}
current_palette




dcrt <- custom_ratio(d340, d380)


cmb <- list(d340, d380, drt, dcrt)

df_time <- time_col_name(drt2, name_only = T)


length(df_time$Time)

step = 1.62

seq(from = 0,  by=step, length.out = length(df_time$Time))

View(cmb)


lon <- c('Ratio', 'Numerator', 'Denominator', 'Custom_ratio')

View(lon)

finlist <- setNames(cmb, lon)
View(finlist)

lob <- c(T, F, F, T)

finlist <- finlist[lob]

typeof(finlist[[1]])

for (id in 1:length(finlist)) {
  
  as.data.frame(finlist[[id]])
}





a <- F
b <- F
c <- F
d <- F

if (!(a & b & c & d)) {print(T)}
