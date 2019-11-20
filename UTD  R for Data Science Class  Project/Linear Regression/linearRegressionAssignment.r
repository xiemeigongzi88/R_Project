caliHouse <- read.csv2(file="housing.csv", header=TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
View(caliHouse)
str(caliHouse)
summary(caliHouse)

#To find out which col has nulls
for(i in 1:ncol(caliHouse)) {
  colName <- colnames(caliHouse[i])
  pctNull <- sum(is.na(caliHouse[,i]))/length(caliHouse[,i])
  if (pctNull > 0.0) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
  }
}

#Replaces nulls with medians
caliHouse$total_bedrooms[is.na(caliHouse$total_bedrooms)] = median(caliHouse$total_bedrooms, na.rm = TRUE)

#To give a histogram of everyone
library(reshape2)
ggplot(data = melt(caliHouse), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

#Correlation
require(corrplot)
M <- cor(caliHouse[,1:9])
corrplot(M, method = "circle")
corMat = as.data.frame(corrplot(M, method = "number"))
names(corMat) <- names(caliHouse[,1:9])

#correlation with an attribute
row.names(corMat)[abs(corMat$median_house_value) > 0.50]

#Predictions
attach(caliHouse)
lm.fit=lm(median_house_value~median_income)
plot(lm.fit)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

lm.fit=lm(median_house_value~.,data=caliHouse)
summary(lm.fit)

lm.fit=lm(median_house_value~.-ocean_proximity,data=caliHouse)
summary(lm.fit)
