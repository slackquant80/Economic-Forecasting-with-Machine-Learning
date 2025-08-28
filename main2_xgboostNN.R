######################## Xgboost ##########################
# Import the Boosting function

# setwd("C:/Users/SKKU/Desktop/main")   
# dir()

# load("rawdata.rda")
# Y = dados

#install.packages("devtools")  
#library(devtools)  
#install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)

library(tidyverse)

data <- read.csv("DataKoreaFrom200408updated.csv")

tcode = data[1,]  # first element: Transformation code
tcode

### Transformation 

data = data[-1,]

tdata = data[-(1:2),]

head(tdata[,1:5])


for (i in 2:94){
  
  if(tcode[i] == 1){
    tdata[,i] <- data[-(1:2),i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data[-1,i])
  } # 1차 차분
  
  # tcode 3은 없음(2차 차분)
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data[-(1:2),i])
  } # log
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data[-1,i]))
  } # log differencing
  
  if(tcode[i] == 6){
    tdata[,i] <- diff(diff(log(data[,i])))
  } # log 취한 뒤 2차 차분
  
  if(tcode[i] == 7){
    tdata[,i] <- diff(data[-1,i]/data[1:(nrow(data)-1),i])
  } # 증가율의 1차 차분
}

Y <- tdata %>%
  select(date, CPI, everything())

rownames(Y) <- Y$date

Y <- Y %>% 
  select(-date)
Y <- as.matrix(Y)

str(Y)


#install.packages("xgboost")
library(xgboost)

source('functions/func-xgb.R')
npred = 90

xgb1=xgb.rolling.window(Y,npred,1,1)
xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
#xgb9=xgb.rolling.window(Y,npred,1,9)
xgb12=xgb.rolling.window(Y,npred,1,12)

# Get the error
xgb.rmse = cbind(xgb1$errors[1], xgb3$errors[1], xgb6$errors[1], xgb12$errors[1])
xgb.mae = cbind(xgb1$errors[2], xgb3$errors[2], xgb6$errors[2], xgb12$errors[2])

# Get the prediction
xgb.pred = cbind(xgb1$pred, xgb3$pred, xgb6$pred, xgb12$pred)


######################## Neural Networks(Deep Learning) ##########################
source("functions/func-nn.R")

#install.packages("dplyr")
library(dplyr)
#install.packages("keras")
library(keras)

#install.packages("h2o")
library(h2o)
h2o.init()

nn1=nn.rolling.window(Y,npred,1,1)
nn3=nn.rolling.window(Y,npred,1,3)
nn6=nn.rolling.window(Y,npred,1,6)
#nn9=nn.rolling.window(Y,npred,1,9)
nn12=nn.rolling.window(Y,npred,1,12)

# Get the error
nn.rmse = cbind(nn1$errors[1], nn3$errors[1], nn6$errors[1], nn12$errors[1])
nn.mae = cbind(nn1$errors[2], nn3$errors[2], nn6$errors[2], nn12$errors[2])

# Get the prediction
nn.pred = cbind(nn1$pred, nn3$pred, nn6$pred, nn12$pred)


# saving entire workspace
save.image("results_forecasts(2).RData")    
