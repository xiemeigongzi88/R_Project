# R Linear Regression Project for housing price in R 
# Original R Code 

View(housing)

data<-housing

colnames(data)

summary(data)

sum(is.na(data))

# bird view
library("ggplot2")
ggplot(data=melt(data),mapping = aes(x=value))+geom_histogram(bins=30)+facet_wrap(~variable,scales='free_x')


# step 2 
data$total_bedrooms[is.na(data$total_bedrooms)]=median(data$total_bedrooms,na.rm = TRUE)

sum(is.na(data)) # check the number of NA 

#Fixing the total columns
data$mean_bedrooms=data$total_bedrooms/data$households

data$mean_rooms=data$total_rooms/data$households

drop=c("total_bedrooms","total_rooms")

data=data[,!(names(data) %in% drop)]

#View(data)


#Take categoricals into Booleans 
categories=unique(data$ocean_proximity)
#split the category 
cat_housing=data.frame(ocean_proximity=data$ocean_proximity)


for(cat in categories)
{
  cat_housing[,cat]=rep(0,times=nrow(cat_housing))
}


for(i in 1:length(cat_housing$ocean_proximity))
{
  cat=as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i]=1
}

#View(cat_housing)

cat_columns=names(cat_housing)
keep_columns=cat_columns[cat_columns!='ocean_proximity']
cat_housing=select(cat_housing,one_of(keep_columns))

#View(cat_housing)

########################################################
# numberic dataset 
colnames(data)

drops=c("ocean_proximity","median_house_value")

data_num=data[,!(names(data) %in% drops)]

View(data_num)

unscaled_housing=cbind(cat_housing,data_num,data$median_house_value)

View(unscaled_housing)

#sum(is.na(unscaled_housing)) # check the number of NA 

scaled_housing_num=scale(data_num)

cleaned_housing=cbind(cat_housing,scaled_housing_num,median_house_value=data$median_house_value)

View(cleaned_housing)

###############################################
# Step 3. Linear Regression 
# performing Multiple Linear Regression 
model<-lm(cleaned_housing$median_house_value~cleaned_housing$`NEAR BAY`+cleaned_housing$INLAND+cleaned_housing$`NEAR OCEAN`+cleaned_housing$ISLAND+cleaned_housing$longitude+cleaned_housing$latitude+cleaned_housing$housing_median_age+cleaned_housing$population+cleaned_housing$households+cleaned_housing$median_income+cleaned_housing$mean_bedrooms+cleaned_housing$mean_rooms)

# Getting Regression Statistics 
anova(model) # get anova table 

coefficients(model)  # model coefficients 

coef(model)  # same as coefficients() function 

confint(model) # confidentce intervals for the regression coefficients 

deviance(model)  # residual sum of squares 

effects(model) # Vector of orthogonal effects 

fitted(model)  # Vector of fitted y values 

residuals(model)  # Model residules 

resid(model) # same as residuals() function

vcov(model)  # variance-covariance matrix of the main paprameters 


##############################################################
# Selecting the best regression variables 

full_model<-lm(cleaned_housing$median_house_value~cleaned_housing$`NEAR BAY`+cleaned_housing$INLAND+cleaned_housing$`NEAR OCEAN`+cleaned_housing$ISLAND+cleaned_housing$longitude+cleaned_housing$latitude+cleaned_housing$housing_median_age+cleaned_housing$population+cleaned_housing$households+cleaned_housing$median_income+cleaned_housing$mean_bedrooms+cleaned_housing$mean_rooms)
summary(full_model)

reduced_mode<-step(full_model,direction = "backward")
summary(reduced_mode)

min_model<-lm(cleaned_housing$median_house_value~cleaned_housing$`NEAR BAY`)
fwd_model<-step(min_model,direction="forward",scope = (~cleaned_housing$`NEAR BAY`+cleaned_housing$INLAND+cleaned_housing$`NEAR OCEAN`+cleaned_housing$ISLAND+cleaned_housing$longitude+cleaned_housing$latitude+cleaned_housing$housing_median_age+cleaned_housing$population+cleaned_housing$households+cleaned_housing$median_income+cleaned_housing$mean_bedrooms+cleaned_housing$mean_rooms))
summary(fwd_model)

# Forming Confidence Intervals for Regression 
model<-lm(cleaned_housing$median_house_value~cleaned_housing$`NEAR BAY`+cleaned_housing$INLAND+cleaned_housing$`NEAR OCEAN`+cleaned_housing$ISLAND+cleaned_housing$longitude+cleaned_housing$latitude+cleaned_housing$housing_median_age+cleaned_housing$population+cleaned_housing$households+cleaned_housing$median_income+cleaned_housing$mean_bedrooms+cleaned_housing$mean_rooms)

confint(model)

confint(model, level=0.99)

plot(model,which = 1)

#Identifying influential observations 
influence.measures(model)

cov(cleaned_housing)


hist(cleaned_housing$median_house_value)

names<-colnames(cleaned_housing)


##############################################################
# step 4. Data Vision 
require(corrplot)

res<-cor(cleaned_housing)
round(res,2)
corrplot(res,method="circle",tl.cex = 0.7)
# cor() Can only calculate the correlation coefficient, can not give a significant book review p-value
# rcorr() can give correlation coefficient and significance level at the same time
corrplot.mixed(res,tl.cex=0.7,number.cex=0.7) 

# change color 
col<-colorRampPalette(c("red","black","blue"))(20)
corrplot(res,order="hclust",addrect = 2,col=col)

# Set the correlation result that did not pass the statistics to play X

res4 <- cor.mtest(cleaned_housing, conf.level = .95)
corrplot(res, tl.cex=0.7,p.mat = res4$p, sig.level = .01)

#还可设置不显著的空白，或显示p值；更可以利用此方法显示所有p值，或用*数量代表显示性

## leave blank on no significant coefficient
corrplot(res, tl.cex=0.001, p.mat = res4$p, insig = "blank")

## add p-values on no significant coefficient
corrplot(res, tl.cex=0.001,p.mat = res4$p, insig = "p-value")

## add all p-values
corrplot(res, tl.cex=0.01,p.mat = res4$p, insig = "p-value", sig.level = -1)

## star level
corrplot(res, tl.cex=0.01,p.mat = res4$p, insig = "label_sig")


#The output of the function rcorr() is a list containing the following elements : - r : the correlation matrix - n : the matrix of the number of observations used in analyzing each pair of variables - P : the p-values corresponding to the significance levels of correlations.


# Visual correlation coefficient matrix
symnum(res,abbr.colnames = FALSE)

corrplot(res, tl.cex = 0.7,type = "upper", order = "hclust",  tl.srt = 90)

# Draw with p-value
#corrplot(res$r, tl.cex = 0.7,type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")


library(PerformanceAnalytics)#Loading package
#Shown on the diagonal is the distribution map
#The lower left shows a bivariate scatter plot with a fitted line.
#The upper right shows the correlation coefficient and the level of significance.
#chart.Correlation(cleaned_housing, histogram=TRUE, pch=19)

#Visualization of a Correlation Matrix. On top the (absolute) value of the correlation plus the result of the cor.test as stars. On bottom, the bivariate scatterplots, with a fitted line
chart.Correlation(unscaled_housing, histogram=TRUE, pch=10)

#These functions are useful for converting hand-designed `sequential' or `diverging' color schemes into continous color ramps eg for image and filled contour plots. Unfortunately the interpolation is still in RGB space.
col<- colorRampPalette(c("blue", "white", "red"))(20)#调用颜色版自定义颜色

#A heat map is a false color image (basically image(t(x))) with a dendrogram added to the left side and to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.
heatmap(x = res, col = col, symm = TRUE)#symm表示是否对称

pairs(cleaned_housing[names])

#SPLOM, Histograms And Correlations For A Data Matrix
install.packages("psych")
library("psych")
pairs.panels(cleaned_housing[names])

plot(cleaned_housing)
