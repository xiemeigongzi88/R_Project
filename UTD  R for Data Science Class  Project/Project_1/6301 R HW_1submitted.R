# R 6301 sxw171930  Shuang Wang 





## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----cars----------------------------------------------------------------
car_worth_train <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/CarWorth_Train.csv")

# Find out the dimensions of this dataset
dim(car_worth_train)

# Find out the column names
ncol(car_worth_train) #
colnames(car_worth_train)

# Use the View() function to look at the dataset
View(car_worth_train)


# Output column with the name "Model" 
car_worth_train$Model

# Display the summary of the "Price"" column
summary(car_worth_train$Price)


# Create a histogram of the "Price" column
hist(car_worth_train$Price)


## ------------------------------------------------------------------------
sum(car_worth_train[,"Cylinder"] > 4)


## ------------------------------------------------------------------------
# Find count of rows where the \textit{Doors} value is greater than 2
sum(car_worth_train$Doors>2)

# Find count of rows where the \textit{Cruise} value is equal to 1
sum(car_worth_train$Cruise==1)

# Are there any rows where the \textit{Cruise} value is null. Hint: use is.na function
sum(is.na(car_worth_train$Cruise))


## ----ames----------------------------------------------------------------
ames_housing <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/AmesHousing.csv")

View(ames_housing)

# Find percent of nulls in each column
for(i in 1:ncol(ames_housing)) {
  colName <- colnames(ames_housing[i])
  pctNull <- sum(is.na(ames_housing[,i]))/length(ames_housing[,i])
  if (pctNull > 0.50) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
  }
}

## ------------------------------------------------------------------------
# create a data frame
df <- data.frame(
  a = c(1, 2, 3),
  b = c(4, 5, 6),
  c= c(7, 8, 9)
)
# let's drop column c
df$c <- NULL
df

## ------------------------------------------------------------------------
# Drop columns as specified above

ames_housing$Neighborhood<-NULL

## ------------------------------------------------------------------------
# Get rid of rows with null values and call the clean dataset as ames_housing_clean
ames_housing_clean<-na.pass(ames_housing)


## ------------------------------------------------------------------------
# Create a plot as required above

## ------------------------------------------------------------------------
data()
View(ames_housing_clean)

hist(ames_housing_clean$SalePrice,breaks=10)

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



