###############################################################################
### Forecasting Inflation in a Data Rich Environment (Medeiros et al, 2019) ###

### = Replicating TABLE S.12 - Forecasting Errors for CPI (1990-2000) = 

### Boruta Algorithm

###############################################################################


#setwd("C:/Users/HeejoonHan/Desktop/0MLandFEF/Rcodes/main")    

library(randomForest)
library(MCS)

#load("results_forecasts.RData")  #forecasts from Inflation_1990-2000_Forecasting.R 
#real=tail(Y[,1],132)


########################################################
### = Ranking Variables using the Boruta Algorithm = ###
########################################################


#install.packages("Boruta")
library(Boruta)
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


Y2 = Y                  ## Using the Whole Sample 
# Y2 = Y[1:359,]        ## Using only the First Window

# Example for lag setting
lag_1 = 1
lag_3 = 3
lag_6 = 6
lag_12 = 12

aux_1 = embed(Y2,4+lag_1)
y_1 = aux_1[,1]
X_1 = aux_1[,-c(1:(ncol(Y2)*lag_1))]

aux_3 = embed(Y2,4+lag_3)
y_3 = aux_3[,1]
X_3 = aux_3[,-c(1:(ncol(Y2)*lag_3))]

aux_6 = embed(Y2,4+lag_6)
y_6 = aux_6[,1]
X_6 = aux_6[,-c(1:(ncol(Y2)*lag_6))]

aux_12 = embed(Y2,4+lag_12)
y_12 = aux_12[,1]
X_12 = aux_12[,-c(1:(ncol(Y2)*lag_12))]


# load("results_Boruta12.RData") # 아래 명령은 시간이 오래 걸림

boruta_1 <- Boruta(X_1, y_1, maxRuns = 100)
boruta_3 <- Boruta(X_3, y_3, maxRuns = 100)
boruta_6 <- Boruta(X_6, y_6, maxRuns = 100)
boruta_12 <- Boruta(X_12, y_12, maxRuns = 100)
  
plot_1 = plot(boruta_1)
plot_1
plot_3 = plot(boruta_3)
plot_3
plot_6 = plot(boruta_6)
plot_6
plot_12 = plot(boruta_12)
plot_12


attstats_1 = attStats(boruta_1)
attstats_1
# Imp -> z스코어
attstats_3 = attStats(boruta_3)
attstats_3
attstats_6 = attStats(boruta_6)
attstats_6
attstats_12 = attStats(boruta_12)
attstats_12

#write.csv(attstats,"Boruta_12.csv",sep=";",row.names = FALSE, col.names = FALSE)

order_1 = order(attstats_1$meanImp, decreasing = T)   # 평균을 기준으로 내림차순 정렬
# 중요도가 높은 순서대로
order_3 = order(attstats_3$meanImp, decreasing = T)
order_6 = order(attstats_6$meanImp, decreasing = T)
order_12 = order(attstats_12$meanImp, decreasing = T)

order_1
order_3
order_6
order_12

## Cross Validation for Optimal Number of Variables # (Up to 68 Variables)

Errors_1 = rep(NA, 20)          
Errors_3 = rep(NA, 20)
Errors_6 = rep(NA, 20)
Errors_12 = rep(NA, 20)

# load("Boruta_result.RData")  #아래 loop 시간이 오래 걸림
 
for (i in 2:20){
  selected_1 = order_1[1:i]
  model = randomForest(X_1[, selected_1], y_1, importance = TRUE)
   
  pred = model$predicted     
  error = mean((pred - y_1) ^ 2)
  
  Errors_1[i] <- error
}
 
plot(c(1:20), Errors_1, xlab = "# of Variables", ylab = "Fitted Squared Error")

Errors1 = Errors_1

for (i in 2:20){
  selected_3 = order_3[1:i]
  model = randomForest(X_3[, selected_3], y_3, importance = TRUE)
  
  pred = model$predicted     
  error = mean((pred - y_3) ^ 2)
  
  Errors_3[i] <- error
}

plot(c(1:20), Errors_3, xlab = "# of Variables", ylab = "Fitted Squared Error")

Errors3 = Errors_3

for (i in 2:20){
  selected_6 = order_6[1:i]
  model = randomForest(X_6[, selected_6], y_6, importance = TRUE)
  
  pred = model$predicted     
  error = mean((pred - y_6) ^ 2)
  
  Errors_6[i] <- error
}

plot(c(1:20), Errors_6, xlab = "# of Variables", ylab = "Fitted Squared Error")

Errors6 = Errors_6

for (i in 2:20){
  selected_12 = order_12[1:i]
  model = randomForest(X_12[, selected_12], y_12, importance = TRUE)
  
  pred = model$predicted     
  error = mean((pred - y_12) ^ 2)
  
  Errors_12[i] <- error
}

plot(c(1:20), Errors_12, xlab = "# of Variables", ylab = "Fitted Squared Error")

Errors12 = Errors_12

# Rolling Window with Selected Variables

varOrder_1 = order(attstats_1$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors1)                                    # Optimal Number of Variables 
selected_1 = varOrder_1[1:which.min(Errors1)]             # The Set of Optimal Number of Variables
# selected = varOrder[1:8]    # 꺾이는 부분. 8개까지만 사용 

varOrder_3 = order(attstats_3$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors3)                                    # Optimal Number of Variables 
selected_3 = varOrder_3[1:which.min(Errors3)]

varOrder_6 = order(attstats_6$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors6)                                    # Optimal Number of Variables
selected_6 = varOrder_6[1:which.min(Errors6)]

varOrder_12 = order(attstats_12$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors12)                                    # Optimal Number of Variables 
selected_12 = varOrder_12[1:which.min(Errors12)]


source("func-rf_selected2022.R")

npred = 90

rf1_selected = rf.rolling.window(Y2,npred,1,1,selected_1)
rf3_selected = rf.rolling.window(Y2,npred,1,3,selected_3)
rf6_selected = rf.rolling.window(Y2,npred,1,6,selected_6)
rf12_selected = rf.rolling.window(Y2,npred,1,12,selected_12)

# Get the error
rf_selected.rmse = cbind(rf1_selected$errors[1], rf3_selected$errors[1], rf6_selected$errors[1], rf12_selected$errors[1])

rf_selected.mae = cbind(rf1_selected$errors[2], rf3_selected$errors[2], rf6_selected$errors[2], rf12_selected$errors[2])

# Get the prediction
rf_selected.pred = cbind(rf1_selected$pred, rf3_selected$pred, rf6_selected$pred, rf12_selected$pred)

# saving entire workspace
save.image("results_forecasts(4).RData")







#library(foreach)
#install.packages("doSNOW")
#library(doSNOW)
#registerDoSNOW(makeCluster(3, type="SOCK")) # For parallel computing, core 3개인 경우임

# parallel computing이 된다면, func-rf_selected2022.R 파일의 15번째 줄인 
# model=randomForeast(X[,selected],y,importance=TRUE) 대신
# model <- foreach(ntree = rep(167, 3), .combine=randomForest::combine, .multicombine=TRUE, .packages = "randomForest") %dopar% randomForest(X[,selected],y,ntree=ntree, importance=TRUE) 로 수정해서 돌릴 것

######  
# install.packages("Rccp")  # 문제가 있을때 추가 패키지