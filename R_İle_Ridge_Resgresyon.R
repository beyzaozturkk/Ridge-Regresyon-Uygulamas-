### RIDGE REGRESYON ####
sales_data<-read.csv('sales_data.csv' , header = TRUE , sep = "," , dec = ".")
View(sales_data)
nrow(sales_data)

library(tidyverse)
library(caret)
library(glmnet)
library(tidyverse)
library(VIM)
library(mice)

sales_data1<-sales_data%>% select(Item_Weight,Item_Fat_Content,Item_Visibility,Item_Type,Item_MRP,Outlet_Size,Outlet_Type,Outlet_Location_Type,Item_Outlet_Sales)
View(sales_data1)
names(sales_data1)

class(sales_data1$Item_Type)
class(sales_data1$Outlet_Type)
class(sales_data1$Outlet_Location_Type)
class(sales_data1$Outlet_Size)

modelData <- sales_data1 %>% 
  mutate( Item_Fat_Content = as.factor(Item_Fat_Content),Item_Type=as.factor(Item_Type),Outlet_Size=as.factor(Outlet_Size),Outlet_Type=as.factor(Outlet_Type),Outlet_Location_Type=as.factor(Outlet_Location_Type)) %>%
  select(Item_Weight,Item_Fat_Content , Item_Visibility ,Item_Type,Item_MRP,Outlet_Size,Outlet_Type,Outlet_Location_Type,Item_Outlet_Sales)

nrow(modelData)

View(modelData)

modelData$Outlet_Size[sales_data1$Outlet_Size %in% c("", "unknown", "?")] <- NA
md.pattern(modelData)


fig <- aggr(modelData , col = c("orange" , "red") , labels = names(modelData),
            numbers = TRUE , sortVars = TRUE, cex.axis = 0.6 , 
            ylab(c("Histogram of Missing Values" , "Pattern"))
)

unique(sales_data1$Item_Fat_Content)
unique(sales_data1$Item_Type)
unique(sales_data1$Outlet_Type)
unique(sales_data1$Outlet_Location_Type)
unique(sales_data1$Outlet_Size)


modelData$Item_Fat_Content <- gsub("low fat", "Low Fat",modelData$Item_Fat_Content)
modelData$Item_Fat_Content <- gsub("LF", "Low Fat", modelData$Item_Fat_Content)
modelData$Item_Fat_Content <- gsub("reg", "Regular", modelData$Item_Fat_Content)



modelData$Outlet_Size<-factor(sales_data1$Outlet_Size , 
                             levels = c("Small" , "Medium" , "High"),
                             ordered = T
)




?mice
imputed <- mice(data = modelData , m = 3 , maxit = 3 , 
                method = NULL , 
                defaultMethod = c("pmm" ,"lasso.norm","polyreg","polr"))
summary(imputed)
names(imputed)
imputed$m
imputed$imp
imputed$imp$Item_Weight
imputed$imp$Outlet_Size


## 3. imputation degerleri ile veri setini doldur. 
imputedData <- complete(imputed , 3)
View(imputedData)


## One Hot Encoding Dummy Degisken

#n??merik degiskenlerde bir degisiklik olmaz

modelData1<- model.matrix(Item_Outlet_Sales ~.  , data  = imputedData)
head(modelData1)


#train set ve test set bolme

set.seed(145)
train_test_set<- sample(1:nrow(modelData1),size=0.80*nrow(modelData1))

trainsetx<-modelData1[train_test_set,]
testsetx<-modelData1[-train_test_set,]

trainsety<- imputedData$Item_Outlet_Sales[train_test_set]
testsety<- imputedData$Item_Outlet_Sales[-train_test_set]

#ridge regresyon

#alpha, ridge regresyon icin sifir degelerini alir.
#lambda 0.05 oraninda katsayilar guncellensin denir
?glmnet

model1<-glmnet(trainsetx,trainsety,alpha=0,lambda = 0.05)
summary(model1)

model1$a0 # intercept
model1$beta #katsayilara bakilir
model1$df
model1$dim
model1$lambda
model1$dev.ratio # bir nevi r kare olarak yorumlanabilir.
model1$nulldev

#cross validation ile lambda degeri atama

?cv.glmnet

#birden fazla lambda degerleri girmemiz gerekiyor.

model_cross_valid<-cv.glmnet(trainsetx,trainsety,alpha=0,lambda = 10^seq(from=3,to=-2,by=-0.01),nfolds = 10)
model_cross_valid$cvm


plot(model_cross_valid)


#ideal lambda degeri, en iyi hata sonucunu bu lambda degerinde verir
model_cross_valid$lambda.min

# sifir olan katsayilara bakalim

model_cross_valid$nzero #26 beta katsayimiz vardi??.. yani sifir olan hehrangi bir degisken yok
ncol(trainsetx) # intercept ile 27 olur

model_cross_valid #measure=MSE


#model tahmin persormansi

gl<-glmnet(trainsetx,trainsety,alpha=0,lambda = 0.9332543)

prediction_model<-predict(gl,testsetx)

library(caret)

R2(prediction_model,testsety)
MAE(prediction_model,testsety)
RMSE(prediction_model,testsety)

data_predict<- data.frame("predictions"=prediction_model,
                          "actuals"=testsety)

library(formattable)
formattable(data_predict)
