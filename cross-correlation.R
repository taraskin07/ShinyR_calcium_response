cell_1 <- c(5, 15, 20, 27, 23, 20, 17, 12, 9, 5, 4, 6, 6, 7, 6, 5, 6)
cell_2 <- c(27, 23, 24, 38, 45, 63, 48, 40, 35, 30, 25, 25, 30, 25, 25, 24, 23)
cell_3 <- c(128, 130, 126, 125, 140, 148, 165, 145, 141, 137, 132, 128, 129, 134, 133, 131, 132)
cell_4 <- c(228, 230, 228, 230, 226, 225, 240, 248, 265, 245, 241, 237, 239, 234, 240, 238, 236)
cell_5 <- c(67, 66, 63, 69, 67, 66, 63, 69, 89, 100, 115, 102, 90, 80, 70, 69, 69)


ccrr <- ccf(cell_5, cell_3)
ccrr$acf
data_table <- data.frame(ACF=ccrr$acf, Lag=ccrr$lag, N=ccrr$n.used)



c5 <- c(67, 66, 63, 69, 89, 100, 115, 102, 90, 80, 70, 69, 69)
c3 <- c(128, 130, 126, 125, 140, 148, 165, 145, 141, 137, 132, 128, 129)


mgmt <- ccf(c5, c3)
data_table <- data.frame(ACF=mgmt$acf, Lag=mgmt$lag, N=mgmt$n.used)

length(c5)
length(c3)


x_av <- mean(cell_5)
y_av <- mean(cell_3)

x_av <- mean(c5)
y_av <- mean(c3)


xsk <- c5-x_av
xsk
ysk <- c3-y_av

a <- sum(xsk*ysk)
b <- sqrt(sum(xsk*xsk))
b
c <- sqrt(sum(ysk*ysk))

a/(b*c)
