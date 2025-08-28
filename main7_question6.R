
load("results_forecasts_main1.RData")
load("results_forecasts_main2.RData")
load("results_forecasts_main3.RData")
load("results_forecasts_main4.RData")
load("results_forecasts_main5.RData")

# Y가 target variable (indice = 1)
# Y를 rolling window procedure의 window 크기에 맞춘 마지막 window로 설정
# -> 이 작업 해줄 것

############################################# RMSE 기준
##################################################

############################################## horizon6, 12 - the best model is BS_RF(Boruta + Random Forest)

library(randomForest)
library(MCS)
library(Boruta)
library(HDeconometrics)
library(tidyverse)

Y2 = Y[91:210,]        ## 예측을 위한 window 

source("func-rf_selected2022.R")

horizon_6 = runrf(Y2, indice = 1, lag = 6, selected_6)$pred
horizon_12 = runrf(Y2, indice = 1, lag = 12, selected_12)$pred

horizon_6
horizon_12


###################### Bind

# RMSE_pred = cbind(RMSE_horizon_6, RMSE_horizon_12)
# MAE_pred = cbind(MAE_horizon_6, MAE_horizon_12)
# 
# write.csv(RMSE_pred, 'result/RMSE_pred.csv')
# write.csv(MAE_pred, 'result/MAE_pred.csv')


