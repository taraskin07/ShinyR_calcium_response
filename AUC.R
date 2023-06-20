






x = 0
y = 8 - (2*x-4)^2
y



x <- seq(0,4, length = 50)
x
y = 4 - (2*x-4)^2
y





plot(x, y, asp = 0.25)


library('DescTools')
AUC(x,y, from = 1, to = 3, method = 'trapezoid')
AUC(x,y, from = 1, to = 3, method = 'step')
AUC(x,y, from = 0, to = 4, method = 'spline')
AUC(x,y, from = 0, to = 4, method = 'spline', absolutearea = T)



c("trapezoid", "step", "spline", "linear")

library('geiger')


dat <- data.frame(x = x, y = y)

plot(dat)


dat$x


my_ifelse <- function(val, low, up) {
  
  if (val >= up) {return(up)
  } else if (val <= low) {return(low)
      } else {return(val)} 
}

vifelse <- Vectorize(my_ifelse)


ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_line(aes(x=x, y=y))+
  geom_polygon(data = dat, aes(x=vifelse(x, 1.5, 2.5), y=ifelse(x>1.5 & x<2.5, y, 0)), fill="#02d1fa") +
  xlim(0, 4)


library('randomcoloR')

randomColor(2)



