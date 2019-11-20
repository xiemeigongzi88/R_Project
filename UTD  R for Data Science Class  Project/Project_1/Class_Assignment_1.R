## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----cars----------------------------------------------------------------
car_worth_train <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/CarWorth_Train.csv")

# Find out the dimensions of this dataset

# Find out the column names

# Use the View() function to look at the dataset


# Output column with the name "Model" 


# Display the summary of the "Price"" column


# Create a histogram of the "Price" column


## ------------------------------------------------------------------------
sum(car_worth_train[,"Cylinder"] > 4)


## ------------------------------------------------------------------------
# Find count of rows where the \textit{Doors} value is greater than 2

# Find count of rows where the \textit{Cruise} value is equal to 1

# Are there any rows where the \textit{Cruise} value is null. Hint: use is.na function



## ----ames----------------------------------------------------------------
ames_housing <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/AmesHousing.csv")

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



## ------------------------------------------------------------------------
# Get rid of rows with null values and call the clean dataset as ames_housing_clean


## ------------------------------------------------------------------------
# Create a plot as required above

## ------------------------------------------------------------------------
data()

