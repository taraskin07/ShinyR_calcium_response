
source("../../engine.R")




test_that("list_to_save", {
  

  expect_equal(list_to_save(c(1, 2, 3), c('one', 'two', 'three'), c(T, F, T)), c('one' = 1, 'three' = 3))
  
  
  expect_error(list_to_save(c(1, 3), c('one', 'two', 'three'), c(T, F, T)), "List's length are not equal!", fixed=TRUE)
  expect_error(list_to_save(c(1, 2, 3), c('one', 'three'), c(T, F, T)), "List's length are not equal!", fixed=TRUE)
  expect_error(list_to_save(c(1, 2, 3), c('one', 'two', 'three'), c(T, F)), "List's length are not equal!", fixed=TRUE)

})


