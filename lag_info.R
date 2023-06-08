lower = 130
upper = 350
max_lag = 40
main_cell_number = 30

dt_to_shift <- read_excel("~/Rprojects/Test_files/2023-04-15-mpkCCD002.xlsx", sheet = 'Ratio')


shifted_main_cell_values <- finding_shifted_curve(dt_to_shift, 
                                                  main_cell_number = main_cell_number, 
                                                  lower = lower, 
                                                  upper = upper, 
                                                  max_lag = max_lag)

shifted_main_cell_values


#Resulting dataframe
lag_data <- data.frame(A = character(), B = numeric())
lag_data
nrow(lag_data)
colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])
lag_data






shifted_info2 <- shifting_curves_info(lag_data, dt_to_shift, 
                                     shifted_main_cell_values, 
                                     lower = lower, 
                                     upper = upper, 
                                     max_lag = max_lag)

colnames(shifted_info)[colnames(shifted_info) != 'Cell_name']
