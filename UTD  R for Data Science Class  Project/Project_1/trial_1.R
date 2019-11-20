View(housing)

sum(is.na(housing))
#[1] 207
clean_housing<-housing
sum(is.na(clean_housing))
#[1] 207
new_data<-na.omit(clean_housing)
sum(is.na(new_data))
#[1] 0
View(new_data)
sum(is.na(new_data))

require(MASS)

require(ISLR)

require(corrplot)

names<-colnames(new_data)
count(names)

dim(new_data) # 20433 10 

class(new_data)

class(names)

cor_data<-new_data[1:9]
a<-cor_data[[1]]





df<-data.frame(list(cor_data))
  
cor_summary<-c()
z=1
df<-list()
cor_number<-list()
for( i in c(1:9))
{
  for(j in c(i:9))
  {
    if(i!=j)
    {
     
     cor_summary[z]<-cor(cor_data[[i]],cor_data[[j]])
    # cor_number[z]<-matrix(i,j,nrow = 1)
    # df[z]<-matrix(c(i,j,cor_summary[z]))
     z=z+1
    }
  }
}

z=1
k=1
view_cor<-c()
for(i in c(1:9))
{
  for(j in c(1:9))
  {
    if(i<j)
    {
      
      #cat(cor_summary[z])
      view_cor[k] <- cor_summary[z]
      z=z+1
    }
    else{
      #print(NA)
      view_cor[k] <- NA
    }
    
    k = k+1
  }
  cat('\n')
}





M<-cor(cor_data)

corrplot(M,method="circle")

corMAt<-as.data.frame(corrplot(M,method="number"))





# prediction the pricing 

lm.fit=lm(longitude~latitude,data=cor_data)
print(lm.fit)
















