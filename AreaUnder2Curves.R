



NA_Rotated <- read_excel("~/Rprojects/Test_files/NA-Rotated.xlsx", 
                         sheet = "_rotated")

baseline = T
b_min = 0
b_max = 120
region = T
r_min = 561.5923
r_max = 755
ready = FALSE

ccurve <- ggplotly_render(NA_Rotated, 
                         baseline = baseline, 
                         b_min = b_min, 
                         b_max = b_max, 
                         region = region,
                         r_min = 561.5923,
                         r_max = r_max,
                         ready = ready) +
  scale_color_manual(values='black') 



subsetted <- subset(NA_Rotated, (Time < b_max & Time > b_min))
averb <- mean(subsetted$Average)
average_lm <- lm(Average ~ Time, data = subsetted)
cf <- coef(average_lm)
b = cf[[1]]
print(averb)
print(b)


ccurve2 <- ccurve +
  geom_hline(yintercept=averb, color = "blue")


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
