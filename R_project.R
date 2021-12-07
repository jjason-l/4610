head(realestate)
plot(realestate)

m1.mrt = lm(price ~ MRTdistance, data = realestate)
summary(m1.mrt)
m1.convenience = lm(price ~ convenience, data=realestate)
summary(m1.convenience)
m1.age = lm(price ~ age, data = realestate)
summary(m1.age)
m1.date = lm(price ~ date, data = realestate)
summary(m1.date)
m1.latitude = lm(price ~ latitude, data = realestate)
summary(m1.latitude)
m1.longitude = lm(price ~ longitude, data = realestate)
summary(m1.longitude)

AIC(m1.mrt, m1.convenience, m1.age, m1.date, m1.latitude, m1.longitude)
plot (m1.mrt)

m1.mrt.sq <- lm(price ~ MRTdistance + I(MRTdistance^2), data=realestate)
summary(m1.mrt.sq)
AIC(m1.mrt, m1.convenience, m1.age, m1.date, m1.latitude, m1.longitude, m1.mrt.sq)
plot(m1.mrt.sq)

x <- realestate$MRTdistance
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1) # getting lots of x values to draw the line through
yhat <- predict(m1.mrt.sq, 
                newdata=data.frame(MRTdistance=xmesh))

plot(realestate$MRTdistance, realestate$price, xlab="MRT distance", ylab="price", main="Price vs MRT distance")
abline(m1.mrt, col="limegreen", lwd=2)
lines(xmesh, yhat, col="brown", lwd=2)
legend("topright",  # where to draw the legend
       c("Linear", "Quadratic"), # line names
       lty=c(1,1),  # just leave this as 1 for each line you're drawing)
       lwd=c(2,2),  # line thickness
       col=c("limegreen", "brown") # colors of lines
)

m2.mrt.convenience = lm(price ~ MRTdistance+convenience, data = realestate)
m2.mrt.age = lm(price ~ MRTdistance + age, data = realestate)
m2.lat.long = lm(price ~ latitude +longitude, data = realestate)
m2.mrt.convenience.sq = lm(price ~ MRTdistance + I(MRTdistance^2) + convenience + I(convenience^2), data =  realestate)
summary(m2.mrt.convenience)
summary(m2.mrt.age)
summary(m2.lat.long)
summary (m2.mrt.convenience.sq)
AIC(m2.mrt.convenience, m2.mrt.age, m2.lat.long, m2.mrt.convenience.sq)

plot(m2.mrt.convenience.sq)


