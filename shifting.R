

all_cells2_CleanTable <- read_excel("~/Rprojects/Test_files/all_cells2-CleanTable.xlsx", 
                                    sheet = "Ratio")

start_t_shift <- 0
end_t_shift <- 890
max_lag <- 40

dt_to_shift <- all_cells2_CleanTable


shifted_main_cell_values <- finding_shifted_curve(dt_to_shift, 
                      main_cell_number = 195, 
                      lower = start_t_shift, 
                      upper = end_t_shift, 
                      max_lag = max_lag)


lag_data <- data.frame(A = character(), B = numeric())


colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])

lag_data

shifted_info <- shifting_curves_info(lag_data, dt_to_shift, 
                                     shifted_main_cell_values, 
                                     lower = start_t_shift, 
                                     upper = end_t_shift, 
                                     max_lag = max_lag)



cell <- colnames(dt_to_shift)[31]

main_cell_number <- str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
main_cell_number



shifted_main_cell_values <- finding_shifted_curve(dt_to_shift, main_cell_number, start_t_shift, end_t_shift, max_lag)






















lag_values_df <- shifted_info


