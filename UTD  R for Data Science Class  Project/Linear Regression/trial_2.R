# R Linear Regression Project for house price 
# trial_2 

library(tidyverse)

library(reshape2)

View(housing)
colnames(housing)

#head(housing)

summary(housing)

sum(is.na(housing))

#par(mfrow=c(2,5))
library("ggplot2")
ggplot(data=melt(housing),mapping = aes(x=value))+geom_histogram(bins=30)+facet_wrap(~variable,scales='free_x')


# step 2 
housing$total_bedrooms[is.na(housing$total_bedrooms)]=median(housing$total_bedrooms,na.rm = TRUE)


#Fixing the total columns
housing$mean_bedrooms=housing$total_bedrooms/housing$households
housing$mean_rooms=housing$total_rooms/housing$households

drop=c("total_bedrooms","total_rooms")

housing=housing[,!(names(housing) %in% drop)]

View(housing)

#Take categoricals into Booleans 
categories=unique(housing$ocean_proximity)
#split the category 
cat_housing=data.frame(ocean_proximity=housing$ocean_proximity)


for(cat in categories)
{
  cat_housing[,cat]=rep(0,times=nrow(cat_housing))
}

#View(cat_housing)

for(i in 1:length(cat_housing$ocean_proximity))
{
  cat=as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i]=1
}

#View(cat_housing)

cat_columns=names(cat_housing)
keep_columns=cat_columns[cat_columns!='ocean_proximity']
cat_housing=select(cat_housing,one_of(keep_columns))

View(cat_housing)

colnames(housing)

drops=c("ocean_proximity","median_house_value")

housing_num=housing[,!(names(housing) %in% drops)]

View(housing_num)

unscaled_housing=cbind(cat_housing,housing_num,housing$median_house_value)

View(unscaled_housing)

#sum(is.na(unscaled_housing)) # check the number of NA 

scaled_housing_num=scale(housing_num)

cleaned_housing=cbind(cat_housing,scaled_housing_num,median_house_value=housing$median_house_value)

View(cleaned_housing)





# R Cookbook page 287 
# performing Multiple Linear Regression 

model<-lm(cleaned_housing$median_house_value~cleaned_housing$`NEAR BAY`+cleaned_housing$INLAND+cleaned_housing$`NEAR OCEAN`+cleaned_housing$ISLAND+cleaned_housing$longitude+cleaned_housing$latitude+cleaned_housing$housing_median_age+cleaned_housing$population+cleaned_housing$households+cleaned_housing$median_income+cleaned_housing$mean_bedrooms+cleaned_housing$mean_rooms)

# Getting Regression Statistics 
anova(model) # get anova table 
 
coefficients(model)  # model coefficients 

#coef(model)

confint(model) # confidentce intervals for the regression coefficients 

deviance(model)  # residual sum of squares 

effects(model) # Vector of orthogonal effects 

fitted(model)  # Vector of fitted y values 

residuals(model)  # Model residules 

#resid(model) # same as 


summary(model) # R2 F  the residual standard error 

vcov(model)  # variance-covariance matrix of the main paprameters 


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


require(corrplot)

res<-cor(cleaned_housing)
round(res,2)
corrplot(res,method="circle",
         tl.cex = 0.7)
# cor() 只能计算得出相关系数 无法给出显著性书评 p-value 
# rcorr() 函数能够同时给出相关系数和显著性水瓶 
corrplot.mixed(res,tl.cex=0.7,number.cex=0.7) 

# change color 
col<-colorRampPalette("red","black","blue")
corrplot(res,order="hclust",addrect = 2,col=col)

# 设置没通过统计的相关性结果打X

res4 <- cor.mtest(cleaned_housing, conf.level = .95)
corrplot(res, p.mat = res4$p, sig.level = .01)

#还可设置不显著的空白，或显示p值；更可以利用此方法显示所有p值，或用*数量代表显示性

## leave blank on no significant coefficient
corrplot(res, p.mat = res4$p, insig = "blank")

## add p-values on no significant coefficient
corrplot(res, p.mat = res4$p, insig = "p-value")

## add all p-values
corrplot(res, p.mat = res4$p, insig = "p-value", sig.level = -1)

## star level
corrplot(res, p.mat = res4$p, insig = "label_sig")
     

#The output of the function rcorr() is a list containing the following elements : - r : the correlation matrix - n : the matrix of the number of observations used in analyzing each pair of variables - P : the p-values corresponding to the significance levels of correlations.

library(Hmisc)
res2<-rcorr(as.matrix(cleaned_housing))
print(res2)


# 将相关系数以及显著性水瓶 p-value 整合进一个矩阵中 将自定义一个函数

res3<-rcorr(as.matrix(cleaned_housing))

# 可视化相关系数矩阵
symnum(res,abbr.colnames = FALSE)

corrplot(res, tl.cex = 0.7,type = "upper", order = "hclust",  tl.srt = 90)

# 结合 p-value 绘制
corrplot(res2$r, tl.cex = 0.7,type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")


library(PerformanceAnalytics)#加载包
#对角线上显示的是分布图
#左下部显示的是具有拟合线的双变量散点图
#右上部显示的是相关系数以及显著性水平
chart.Correlation(cleaned_housing, histogram=TRUE, pch=19)

chart.Correlation(unscaled_housing, histogram=TRUE, pch=10)

col<- colorRampPalette(c("blue", "white", "red"))(20)#调用颜色版自定义颜色
heatmap(x = res, col = col, symm = TRUE)#symm表示是否对称

pairs(cleaned_housing[names])

install.packages("psych")
library("psych")
pairs.panels(cleaned_housing[names])


























