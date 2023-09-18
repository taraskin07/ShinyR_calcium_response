ct <- read_excel("~/Current/CA Imaging checkpoints_V2/2023-04-13/No_Shaking_no_AVP_medium_replaced_24h/processed/CleanTable/2023-04-13-mpkCCD001-CleanTable.xlsx", 
                         sheet = "Ratio")

ct2 <- ct[c(1, 12, 101)]

dfts <- ct2
start_time <- 130
end_time <- 330

index1 <- min(which(dfts$Time >= start_time))
index2 <- max(which(dfts$Time <= end_time))

dfts[index1:index2,] 


which.max(dfts[index1:index2,][[1]])



dfts[index1:index2,][13, ]
max_indices <- apply(dfts[index1:index2,], 2, which.max)
max_indices


dfts[index1:index2,][14, ]


initial_indicies <- max_indices + index1 - 1
initial_indicies

dfts[40, 3]



indices_list <- initial_indicies[-1]
indices_list

lowest_value <- min(unlist(indices_list))


lowest_value

trace_name <- names(which.min(unlist(indices_list)))

trace_name

difference <- sapply(indices_list, function(x)
  x - lowest_value)
difference
