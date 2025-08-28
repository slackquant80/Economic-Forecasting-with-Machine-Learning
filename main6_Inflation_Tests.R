###############################################################################
### Forecasting Inflation in a Data Rich Environment (Medeiros et al, 2019) ###
###############################################################################

### Codes for Replicating TABLE S.12 - Forecasting Errors for CPI (1990-2000), 

####################
### Testing Part
####################


#setwd("D:/2Teaching/BOK/ML2021/Rcodes/main") 
#dir()

#install.packages("sandwich")
#install.packages("MCS")

load("results_forecasts_main1.RData")
load("results_forecasts_main2.RData")
load("results_forecasts_main3.RData")
load("results_forecasts_main4.RData")
load("results_forecasts_main5.RData")


################
### DMW test ###
################

# source("dmwtest.R")
# 
# # RW vs RF
# dmwtest_rw_rf = matrix(NA,1,12)
# dmwpvalue = matrix(NA,1,12)
# 
# # dmwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. (통계치가 asymptotic normal을 따르므로) p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함
# 
# real = tail(Y[, 1], 132)  # actual value
# 
# # Try i =1 first
# i =1
# 
# for(i in 1:12){
#   
#   dmw = dmwtest(real, rw_pred[,i], rf_pred[,i])
#   dmwtest_rw_rf[i] <- dmw[1]
#   dmwpvalue[i] <- dmw[2]
# 
# }
# 
# dmwtest_rw_rf  # forecast horizon별 test statistic 결과, difference = rw_pred - rf_pred
# dmwpvalue      # forecast horizon별 p-value  

# DMW는 넘어가고 아래 테스트로~

#############################################################
### = Giacomini-White Test for Equal Predictive Ability = ###
#############################################################

source("gwtest.R")
library(sandwich)

real = tail(Y[, 1], 90)
# real

# RW vs BS_RF
gwtest_rw_bsrf = matrix(NA, 1, 4)
gwpvalue_rw_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(rw_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
    gwtest_rw_bsrf[i] <- gw$statistic
    gwpvalue_rw_bsrf[i] <- gw$p.value
}

gwtest_rw_bsrf   # forecast horizon별 test statistic 결과, difference = rw_pred - bsrf_pred
# 통계치가 (+) 첫번째 모형의 forecast loss가 더 크다
# MAE로 계산되어 있음 -> 프로젝트 할때 코드 그대로 쓰면 됨
gwpvalue_rw_bsrf # forecast horizon별 p-value
# p-value가 0.05보다 작으므로, random walk의 forecast loss가 유의미하게 더 큼
# -> 통계적으로 유의하게 bs_random forest가 예측력이 더 좋음

# AR vs BS_RF
gwtest_ar_bsrf = matrix(NA, 1, 4)
gwpvalue_ar_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(ar_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_ar_bsrf[i] <- gw$statistic
  gwpvalue_ar_bsrf[i] <- gw$p.value
}

gwtest_ar_bsrf  
gwpvalue_ar_bsrf

# Ridge vs BS_RF
gwtest_ridge_bsrf = matrix(NA, 1, 4)
gwpvalue_ridge_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(ridge_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_ridge_bsrf[i] <- gw$statistic
  gwpvalue_ridge_bsrf[i] <- gw$p.value
}

gwtest_ridge_bsrf  
gwpvalue_ridge_bsrf

# LASSO vs BS_RF
gwtest_lasso_bsrf = matrix(NA, 1, 4)
gwpvalue_lasso_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(lasso_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_lasso_bsrf[i] <- gw$statistic
  gwpvalue_lasso_bsrf[i] <- gw$p.value
}

gwtest_lasso_bsrf  
gwpvalue_lasso_bsrf

# adaptive LASSO vs BS_RF
gwtest_adalasso_bsrf = matrix(NA, 1, 4)
gwpvalue_adalasso_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(adalasso_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_adalasso_bsrf[i] <- gw$statistic
  gwpvalue_adalasso_bsrf[i] <- gw$p.value
}

gwtest_adalasso_bsrf  
gwpvalue_adalasso_bsrf

# Elastic Net vs BS_RF
gwtest_elasticnet_bsrf = matrix(NA, 1, 4)
gwpvalue_elasticnet_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(elasticnet_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_elasticnet_bsrf[i] <- gw$statistic
  gwpvalue_elasticnet_bsrf[i] <- gw$p.value
}

gwtest_elasticnet_bsrf  
gwpvalue_elasticnet_bsrf

# adaptive Elastic Net vs BS_RF
gwtest_adaelasticnet_bsrf = matrix(NA, 1, 4)
gwpvalue_adaelasticnet_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(adaelasticnet_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_adaelasticnet_bsrf[i] <- gw$statistic
  gwpvalue_adaelasticnet_bsrf[i] <- gw$p.value
}

gwtest_adaelasticnet_bsrf  
gwpvalue_adaelasticnet_bsrf

# Target Factors vs BS_RF
gwtest_tfact_bsrf = matrix(NA, 1, 4)
gwpvalue_tfact_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(tfact_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_tfact_bsrf[i] <- gw$statistic
  gwpvalue_tfact_bsrf[i] <- gw$p.value
}

gwtest_tfact_bsrf  
gwpvalue_tfact_bsrf

# Random Forest vs BS_RF
gwtest_rf_bsrf = matrix(NA, 1, 4)
gwpvalue_rf_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(rf_pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_rf_bsrf[i] <- gw$statistic
  gwpvalue_rf_bsrf[i] <- gw$p.value
}

gwtest_rf_bsrf  
gwpvalue_rf_bsrf

# NN vs BS_RF
gwtest_nn_bsrf = matrix(NA, 1, 4)
gwpvalue_nn_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(nn.pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_nn_bsrf[i] <- gw$statistic
  gwpvalue_nn_bsrf[i] <- gw$p.value
}

gwtest_nn_bsrf  
gwpvalue_nn_bsrf

# LSTM vs BS_RF
gwtest_lstm_bsrf = matrix(NA, 1, 4)
gwpvalue_lstm_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(lstm.pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_lstm_bsrf[i] <- gw$statistic
  gwpvalue_lstm_bsrf[i] <- gw$p.value
}

gwtest_lstm_bsrf  
gwpvalue_lstm_bsrf

# XGboost vs BS_RF
gwtest_xgb_bsrf = matrix(NA, 1, 4)
gwpvalue_xgb_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(xgb.pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_xgb_bsrf[i] <- gw$statistic
  gwpvalue_xgb_bsrf[i] <- gw$p.value
}

gwtest_xgb_bsrf  
gwpvalue_xgb_bsrf

# CNN vs BS_RF
gwtest_cnn_bsrf = matrix(NA, 1, 4)
gwpvalue_cnn_bsrf = matrix(NA, 1, 4)

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함.

for (i in 1:4) {    # 1, 3, 6, 12-step horizon forecast -> rw_pred, rf_pred : main1 파일에서 저장해놓은 결과값
  gw = gw.test(cnn.pred[, i], rf_selected.pred[, i], real, tau = i, T = 90, method = "NeweyWest")   # standard error 구하는 방식
  
  gwtest_cnn_bsrf[i] <- gw$statistic
  gwpvalue_cnn_bsrf[i] <- gw$p.value
}

gwtest_cnn_bsrf  
gwpvalue_cnn_bsrf

###########################################
### = Model Confidence Set (MCS) Test = ### 
###########################################

library(MCS)


# 아래 loop를 다 돌리는 건 시간이 걸림. Try a specific i(forecasting horizon) first. 
# i=12

# Superior Set Model에 남아 있는 모형들의 예측력이 우수함을 의미함

for (i in 1:4) {
  
  Pred = cbind(rw_pred[,i], ar_pred[,i], ridge_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i], adaelasticnet_pred[,i], tfact_pred[,i], rf_pred[,i], nn.pred[,i], lstm.pred[,i], xgb.pred[,i], rf_selected.pred[,i], cnn.pred[,i])
  
  LOSS = Pred - real
  LOSS1 = LOSS ^ 2      # squared error
  LOSS2 = abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha = 0.5, B = 5000, statistic = "Tmax")  # alpha -> 유의수준, 0.5 -> 50% 유의수준, B -> bootstrap 숫자, Tmax statistic 사용
}
# 10, 12번째 모형이 우월하게 나옴

#?MCSprocedure
# (1-alpha)*100% 수준에서 Model Confidence Set에 포함 (may also try alpha = 0.2 or 0.25)

#SSM_i <- MCSprocedure(LOSS1, alpha=0.2, B=5000, statistic="Tmax")

# saving entire workspace
save.image("results_models_test포함전체.RData")
