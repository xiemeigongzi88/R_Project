# R 6301 sxw171930  Shuang Wang 
data()

airquality_clean<-na.exclude(airquality)

colnames(airquality_clean)

summary(airquality_clean$Ozone)

hist(airquality_clean$Ozone, 
     main="Histogram for the Ozone of Air Airquality", 
     xlab="Ozone", 
     border="blue", 
     col="green",
     xlim=c(0,200),
     las=1, 
     breaks=100)

summary(airquality_clean$Solar.R)
hist(airquality_clean$Solar.R, 
     main="Histogram for the Solar of Air Airquality", 
     xlab="Solar.R", 
     border="blue", 
     col="green",
     xlim=c(0,335),
     las=1, 
     breaks=100)

summary(airquality_clean$Wind)
hist(airquality_clean$Wind, 
     main="Histogram for the Wind of Air Airquality", 
     xlab="Wind", 
     border="blue", 
     col="green",
     xlim=c(0,22),
     las=1, 
     breaks=10)


summary(airquality_clean$Temp)
hist(airquality_clean$Temp, 
     main="Histogram for the Solar Air Airquality", 
     xlab="Temp", 
     border="blue", 
     col="green",
     xlim=c(50,100),
     las=1, 
     breaks=10)


summary(airquality_clean$Day)
hist(airquality_clean$Day, 
     main="Histogram for the Solar Air Airquality", 
     xlab="Day", 
     border="blue", 
     col="green",
     xlim=c(0,35),
     las=1, 
     breaks=15)



