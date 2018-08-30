library(gbm)
library(rpart)
library(randomForest)
library(caret)

setwd("/Users/adititiwari/Desktop/RF_GBM")

weather=read.csv("Data_Set_WU_Actual12sep.csv",header=TRUE, stringsAsFactors = FALSE)
##Reading Demand
load1=read.csv("SCADA_Demand.csv",header=TRUE, stringsAsFactors = FALSE)
load1[,3] <- na.spline(load1[,3])
load1$Date=as.Date(load1$Date, format = "%m/%d/%Y")
weather$Weather_ForeCastDate=as.Date(weather$Weather_ForeCastDate, format = "%d/%m/%Y")
load_new=subset(load1,Date>="2016-07-26")
weather=subset(weather,Weather_ForeCastDate<="2016-09-14")
load_new1=subset(load_new,Date<="2016-09-14")
setDT(weather)
setDT(load_new1)
load_new1<- load_new1[81:4896]

load=log(load_new1[,c(3), with=FALSE])
Temp=weather[,c(7:20)]
humid=weather[,c(21:34)]
cloudcovr=weather[,c(35:48)]
windspeed=weather[,c(49:62)]
winddirection=weather[,c(63:76)]
sunr=weather[,c(77:90)]
#Suns=Weather[,c(91:104)]
#Wea=Weather[,c(105:118)]
#Pre=Weather[,c(119:132)]
#Rain=Weather[,c(133:146)]
#################################################################



temppca <- princomp(weather[,c(7:20),with = FALSE])
weather[, tempcomp := (predict(temppca, newdata = weather[,c(7:20),with = FALSE]))[,1]]
summary(temppca)
screeplot(temppca)
Data.File=weather[,c(4,6), with = FALSE]
Data.File=cbind(Data.File,load,weather$tempcomp)
colnames(Data.File)[3]<-"load"
colnames(Data.File)[4]<-"Temperature"

humidpca<- princomp(weather[,c(21:34),with = FALSE])
weather[, humidcomp := (predict(humidpca, newdata = weather[,c(21:34),with = FALSE]))[,1]]
summary(humidpca)
screeplot(humidpca)
Data.File=cbind(Data.File,weather$humidcomp)
colnames(Data.File)[5]<-"Humidity"

cloudcovrpca<- princomp(weather[,c(35:48),with = FALSE])
weather[, cloudcovrcomp := (predict(cloudcovrpca, newdata = weather[,c(35:48),with = FALSE]))[,1]]
summary(cloudcovrpca)
screeplot(cloudcovrpca)
Data.File=cbind(Data.File,weather$cloudcovrcomp)
colnames(Data.File)[6]<-"CloudCover"

windspeedpca<- princomp(weather[,c(49:62),with = FALSE])
weather[, windspeedcomp := (predict(windspeedpca, newdata = weather[,c(49:62),with = FALSE]))[,1]]
summary(windspeedpca)
screeplot(windspeedpca)
Data.File=cbind(Data.File,weather$windspeedcomp)
colnames(Data.File)[7]<-"Windspeed"

winddirectionpca<-princomp(weather[,c(63:76),with = FALSE])
weather[, winddirectioncomp := (predict(winddirectionpca, newdata = weather[,c(63:76),with = FALSE]))[,1]]
summary(winddirectionpca)
screeplot(winddirectionpca)
Data.File=cbind(Data.File,weather$winddirectioncomp)
colnames(Data.File)[8]<-"WindDirection"
#sunrpca<-princomp(weather[,c(77:90),with = FALSE])
#weather[, sunrcomp := (predict(sunrpca, newdata = weather[,c(77:90),with = FALSE]))[,1]]

###################################################################


Lag1_temp=array(0,nrow(Data.File))
Lag_humid=array(0,nrow(Data.File))
Lag1_humid=array(0,nrow(Data.File))
Lag_cloudcovr=array(0,nrow(Data.File))
Lag1_cloudcovr=array(0,nrow(Data.File))
#Lag3=matrix(0,nrow=nrow(Weather_Jal),ncol=3)
Lag2_load=array(0,nrow(Data.File))
Lag3_load=array(0,nrow(Data.File))
max_load=array(0,nrow(Data.File))
min_load=array(0,nrow(Data.File))


Lag_temp = c(0,0,0,0, weather$tempcomp[1:(nrow(weather)-4)])
Lag1_temp = c(0,0,0,0,0,0,0,0, weather$tempcomp[1:(nrow(weather)-8)])
Lag_humid = c(0,0,0,0, weather$humidcomp[1:(nrow(weather)-4)])
Lag1_humid= c(0,0,0,0,0,0,0,0, weather$humidcomp[1:(nrow(weather)-8)])
Lag_cloudcovr= c(0,0,0,0,weather$humidcomp[1:(nrow(weather)-4)])
Lag1_cloudcovr= c(0,0,0,0,0,0,0,0, weather$humidcomp[1:(nrow(weather)-8)])
Lag2_load = c(rep(0,192), load[1:(nrow(load)-192)]$Demand.Scada)
Lag3_load = c(rep(0,288), load[1:(nrow(load)-288)]$Demand.Scada)
Lag4_load = c(rep(0,384), load[1:(nrow(load)-384)]$Demand.Scada)
Lag5_load = c(rep(0,480), load[1:(nrow(load)-480)]$Demand.Scada)
Lag6_load = c(rep(0,576), load[1:(nrow(load)-576)]$Demand.Scada)
Lag7_load = c(rep(0,672), load[1:(nrow(load)-672)]$Demand.Scada)
max_load = apply(cbind.data.frame(Lag2_load,Lag3_load,Lag4_load,Lag5_load,Lag6_load,Lag7_load),1, max)
min_load = apply(cbind.data.frame(Lag2_load,Lag3_load,Lag4_load,Lag5_load,Lag6_load,Lag7_load),1, min)

data.train=data.frame(Data.File, Lag_temp,Lag1_temp,Lag_humid,Lag1_humid,Lag_cloudcovr,Lag1_cloudcovr,Lag2_load,Lag3_load,max_load,min_load)



start_row<-nrow(weather[Weather_ForeCastDate<=unique(weather$Weather_ForeCastDate)[10]])
j=nrow(data.train)-start_row-192
k=10
RF_GBM_All=matrix(0,96,2)
RF_GBM_All1=matrix(0,1,2)
while((j+start_row)<=(nrow(data.train)-192))
{
  data.train1=data.train[(j+1):(start_row+j-192),]
  data.test1=data.train[(start_row+j+1+96):(start_row+j+192),-c(3)]
  set.seed(316)
  
  
  fitControl <- trainControl(method = "repeatedcv",number =2 ,repeats = 2,selectionFunction = "best")
  gparams <- expand.grid(n.trees = c(50, 100, 200, 500), interaction.depth = c(2, 3, 4),shrinkage = c(0.001, 0.01, 0.1, 0.2), n.minobsinnode = 10)
  rm.fit=train(load~.,data=data.train1,method = "rf",trControl = fitControl)
  test.rmscore=predict(rm.fit,data.test1)
  RF_weights=exp(test.rmscore)
  
  set.seed(316)
  rp.fit=train(load~.,data=data.train1,method = "gbm",tuneGrid = gparams,trControl = fitControl)
  test.rpscore=predict(rp.fit,data.test1)
  GBM_weights=exp(test.rpscore)
  RF_GBM_All=cbind(RF_weights,GBM_weights)
  RF_GBM_All1=rbind(RF_GBM_All1,RF_GBM_All)
  
  print("Models Running...")
  j=j+96
}
write.csv(RF_GBM_All1,"RF_GBM_AllVar_dayahead.csv")

##RF_GAMSpec:No Weather Lags##
##Without Cloud Cover##

j=nrow(data.train)-start_row-192
RF_GBM_No_cloudcovr=matrix(0,96,2)
RF_GBM_No_cloudcovr1=matrix(0,1,2)

while((j+start_row)<=(nrow(data.train)-192))
{
    data.train2=data.train[(j+1):(start_row+j-96),-c(6,9:14)]
    data.test2=data.train[(start_row+j+1):(start_row+j+96),-c(3,6,9:14)]
    set.seed(316)
    
    fitControl <- trainControl(method = "repeatedcv",number =2 ,repeats = 2,selectionFunction = "best")
    rm.fit=train(load~.,data=data.train2,method = "rf",trControl = fitControl)
    test.rmscore=predict(rm.fit,data.test2)
    RF_weights=exp(test.rmscore)
    
    set.seed(316)
    gparams <- expand.grid(n.trees = c(50, 100, 200, 500), interaction.depth = c(2, 3, 4),shrinkage = c(0.001, 0.01, 0.1, 0.2), n.minobsinnode = 10)
    rp.fit=train(load~.,data=data.train2,method = "gbm",tuneGrid= gparams,trControl = fitControl)
    test.rpscore=predict(rp.fit,data.test2)
    GBM_weights=exp(test.rpscore)
    RF_GBM_No_cloudcovr=cbind(RF_weights,GBM_weights)
    RF_GBM_No_cloudcovr1=rbind(RF_GBM_No_cloudcovr1,RF_GBM_No_cloudcovr)
    
    print("Models Running...")
    j=j+96
}
write.csv(RF_GBM_No_cloudcovr1,"RF_GBM_without_cloud_cover.csv")

  
##RF_GAMSpec:1 Lag of weather##

j=nrow(data.train)-start_row-192
RF_GBM_Lag1wthr=matrix(0,96,2)
RF_GBM_Lag1wthr1=matrix(0,1,2)
while((j+start_row)<=(nrow(data.train)-192))
{
    data.train3=data.train[(j+1):(start_row+j-96),-c(10,12,14)]
    data.test3=data.train[(start_row+j+1):(start_row+j+96),-c(3,10,12,14)]
    set.seed(316)
    
    fitControl <- trainControl(method = "repeatedcv",number =2 ,repeats = 2,selectionFunction = "best")
    rm.fit=train(load~.,data=data.train3,method = "rf",trControl = fitControl)
    test.rmscore=predict(rm.fit,data.test3)
    RF_weights=exp(test.rmscore)
    
    set.seed(316)
    gparams <- expand.grid(n.trees = c(50, 100, 200, 500), interaction.depth = c(2, 3, 4),shrinkage = c(0.001, 0.01, 0.1, 0.2), n.minobsinnode = 10)
    rp.fit=train(load~.,data=data.train3,method = "gbm",tuneGrid = gparams,trControl = fitControl)
    test.rpscore=predict(rp.fit,data.test3)
    GBM_weights=exp(test.rpscore)
    RF_GBM_Lag1wthr=cbind(RF_weights,GBM_weights)
    RF_GBM_Lag1wthr1=rbind(RF_GBM_Lag1wthr1,RF_GBM_Lag1wthr)
    
    print("Models Running...")
    j=j+96
}
write.csv(RF_GBM_Lag1wthr1,"RF_GBM_1Lag_of_weather.csv")


##RF_GAMSpec:No WS and WD##

j=nrow(data.train)-start_row-96
RF_GBM_NoWSWD=matrix(0,96,2)
RF_GBM_NoWSWD1=matrix(0,1,2)
while((j+start_row)<=(nrow(data.train)-192))
{
    data.train4=data.train[(j+1):(start_row+j-96),-c(6,7,8)]
    data.test4=data.train[(start_row+j+1):(start_row+j+96),-c(3,6,7,8)]
    set.seed(316)
    
    fitControl <- trainControl(method = "repeatedcv",number =2 ,repeats = 2,selectionFunction = "best")
    gparams <- expand.grid(n.trees = c(50, 100, 200, 500), interaction.depth = c(2, 3, 4),shrinkage = c(0.001, 0.01, 0.1, 0.2), n.minobsinnode = 10)
    rm.fit=train(load~.,data=data.train4,method = "rf",tuneGrid = gparams,trControl = fitControl)
    test.rmscore=predict(rm.fit,data.test4)
    RF_weights=exp(test.rmscore)
    
    set.seed(316)
    rp.fit=train(load~.,data=data.train4,method = "gbm",trControl = fitControl)
    test.rpscore=predict(rp.fit,data.test4)
    GBM_weights=exp(test.rpscore)
    RF_GBM_NoWSWD=cbind(RF_weights,GBM_weights)
    RF_GBM_NoWSWD1=rbind(RF_GBM_NoWSWD1,RF_GBM_NoWSWD)
    
    print("Models Running...")
    j=j+96
}
write.csv(RF_GBM_NoWSWD1,"RF_GBM_NoWSWD1_of_weather.csv")




