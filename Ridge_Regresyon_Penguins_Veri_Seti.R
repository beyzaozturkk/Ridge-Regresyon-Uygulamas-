#######################  RIDGE REGRESYON  ######################

#### Veri On isleme - Standartlastirma #####

penguins<-read.csv('penguins.csv' , header = TRUE , sep = "," , dec = ".")
View(penguins)

names(penguins)
penguins$sex[penguins$sex %in% c("", "unknown", "?")] <- NA

library(VIM)
fig <- aggr(penguins , col = c("orange" , "red") , labels = names(penguins),
            numbers = TRUE , sortVars = TRUE, cex.axis = 0.6 , 
            ylab(c("Histogram of Missing Values" , "Pattern"))
)
fig

library(mice)
md.pattern(penguins)


which(is.na(penguins$species))
which(is.na(penguins$island))
which(is.na(penguins$bill_length_mm))
which(is.na(penguins$bill_depth_mm))
which(is.na(penguins$flipper_length_mm))
which(is.na(penguins$body_mass_g))
which(is.na(penguins$sex))
new_data<-na.omit(penguins)
View(new_data)
nrow(new_data)

library(caret)
library(glmnet)
library(tidyverse)

modelData <- new_data %>% 
  mutate( species = as.factor(species),island=as.factor(island),sex=as.factor(sex)) %>%
  select(bill_length_mm , bill_depth_mm , flipper_length_mm ,body_mass_g,species,island,sex)

View(modelData)
nrow(modelData)

##Standartlastirma islemi 
num_cols <- c("bill_length_mm" , "bill_depth_mm","flipper_length_mm","body_mass_g")
pre_scaled <- preProcess(modelData[, num_cols] , method = c("center" , "scale"))
modelDataScaled <- predict(pre_scaled , modelData)

## Standartlastirilmis veri seti
View(modelDataScaled)

## One Hot Encoding Dummy Degisken

#numerik degiskenlerde bir degisiklik olmaz

modelDataScaled1 <- model.matrix(body_mass_g ~.  , data  = modelDataScaled)
head(modelDataScaled1)


#train set ve test set bolme

set.seed(145)
train_test_set<- sample(1:nrow(modelDataScaled1),size=0.80*nrow(modelDataScaled1))

trainsetx<-modelDataScaled1[train_test_set,]
testsetx<-modelDataScaled1[-train_test_set,]

trainsety<- modelDataScaled$body_mass_g[train_test_set]
testsety<- modelDataScaled$body_mass_g[-train_test_set]

#ridge regresyon

#alpha, ridge regresyon icin sifir degerini alir.
#lambda 0.05 oraninda katsayilar guncellensin denir
?glmnet

model1<-glmnet(trainsetx,trainsety,alpha=0,lambda = 0.05)
summary(model1)

model1$a0 # intercept
model1$beta #katsayilara bakilir
model1$df
model1$dim
model1$lambda
model1$dev.ratio # bir nevi R kare olarak yorumlanabilir
model1$nulldev

#cross validation ile lambda degeri atama

?cv.glmnet

#birden fazla lambda degerleri girmemiz gerekiyor.

model_cross_valid<-cv.glmnet(trainsetx,trainsety,alpha=0,lambda = 10^seq(from=3,to=-2,by=-0.01),nfolds = 10)
model_cross_valid$cvm
plot(model_cross_valid)

#ideal lambda degeri, en iyi hata sonucunu bu lambda degerinde verir
model_cross_valid$lambda.min

# sifirr olan katsayilara bakalim

model_cross_valid$nzero #8 beta katsayimiz vardi. yani sifir olan hehrangi bir degisken yok
ncol(trainsetx) # intercept ile 9 olur

model_cross_valid #measure=MSE


#Model Tahmin Performansi

gl<-glmnet(trainsetx,trainsety,alpha=0,lambda = 0.01)

prediction_model<-predict(gl,testsetx)

library(caret)

R2(prediction_model,testsety)
MAE(prediction_model,testsety)
RMSE(prediction_model,testsety)

prediction_model
testsety

data_predict<- data.frame("predictions"=prediction_model,
                          "actuals"=testsety)

library(formattable)
formattable(data_predict)






