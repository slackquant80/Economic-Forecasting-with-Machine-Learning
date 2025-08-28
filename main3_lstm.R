
# LSTM Function

# ==================================================================
# Install packages and Recall Library

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#library(reshape2)

# install.packages("tensorflow")  # 관리자 권한으로 실행
library(tensorflow)
#install_tensorflow()

#reticulate::py_discover_config()

# install.packages("keras")  # 관리자 권한으로 실행
library(keras) 
#install_keras()

tf$constant("hello")
tf$version

# use_condaenv("r-tensorflow") if error loading tensorflow


# ==================================================================
# Normalization  앞에서 팩터추출할때 한번 썼었음

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))         # 0 ~ 1 값으로 변환(극단치 영향력 줄이기 위해)
}

# Inverse Normalization 
denormalize <- function(x, minval, maxval) {
  x*(maxval-minval) + minval
}

# ==================================================================
# Load Data Set

#getwd()
#dir()

# load("rawdata.rda")
# 
# Y=dados

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



# ==================================================================
# US inflation forecasting example explanation

nprev = 90
indice = 1
lag = 1

comp = princomp(scale(Y,scale=FALSE))           # 팩터 4개 뽑는것 동일
Y2 = cbind(Y,comp$scores[,1:4]) %>% as.data.frame()
Y3 = lapply(Y2, normalize) %>% as.data.frame() %>% as.matrix()   # normalizing Y2

Y2 %>% dim() # 210 97 : 97 variables
Y2 %>% colnames() # Can see CPI Included with 4 Factor Components
Y3 %>% dim() # 210 97 : Same with Y2 since it's only normalized 

aux = embed(Y3, 4 + lag)
aux %>% dim() # 206 485 : Note 485 - 97 = 388 

# Check aux

Y3 %>% head() # CPI
Y3[,1] %>% head() # CPI


# 97 features with 388 lagged features

y=aux[,indice] # This is our target (label) with total 206 observation
X=aux[,-c(1:(ncol(Y3)*lag))]  # ncol(Y3) = 97
# This is our feature with total 206 obs & 388 variables (including lagged)

ncol(Y3)*lag # 97 meaning aux에서 칼럼1 부터 칼럼 97까지 삭제

# 칼럼 98부터는 t-1 값의 X

# Check aux & X

Y3[,1] %>% head()
X[,1] %>% head()
aux[,98] %>% head()


# aux %>% head()
aux %>% dim()

X %>% dim() # 206 388
y %>% length() # 206

X[,1] %>% head()
c(1:(ncol(Y3)*(lag-1)))

nrow(X) # 206 : number of observation

tail(aux,1)[1:ncol(X)]

#embed(x2,3)
#tail(embed(x2,3),1)

tail(aux,1) %>% length() # 485
tail(aux,1)[1:ncol(X)] %>% length() # 388


ncol(aux)

if(lag==1){
  X.out=tail(aux,1)[1:ncol(X)]  
}else{
  X.out=aux[,-c(1:(ncol(Y3)*(lag-1)))]
  X.out=tail(X.out,1)[1:ncol(X)]
}

X.out %>% length() # 388 with is the test set

X %>% dim() # 388
X.out %>% length() # 388
X.out %>% head()

y %>% head() # 0.2918722 : t 시점부터
X[, 1] %>% head() # 0.2774948 0.2918722 : t-1 시점부터
X[, 98] %>% head() # 0.4224629 0.2774948 0.2918722 : t-2 시점부터
X[, 195] %>% head() # 0.2341573 0.4224629 0.2774948 0.2918722 : t-3 시점부터
X[, 292] %>% head() # 0.2922650 0.2341573 0.4224629 0.2774948 0.2918722 : t-4 시점부터

X[, 97] %>% head()
X[, 194] %>% head()
X[, 291] %>% head()
X[, 388] %>% head()

# ===========================================================================
# Change the order of columns to match the 3D-array object
# 필기 참조, LSTM 코드에는 rearrange 필요

X %>% str()

X2 <- X %>% replace(!0, 0) 
X2 %>% dim()  

for(i in 0:96){
  
  # X2[,1] <- X[,1]
  # X2[,2] <- X[,127]
  # X2[,3] <- X[,253]
  # X2[,4] <- X[,379]
  # X2[,5] <- X[,2]
  # X2[,6] <- X[,128]
  
  X2[,(4*i+1)] <- X[,(i+1)]
  X2[,(4*i+2)] <- X[,(i+98)]
  X2[,(4*i+3)] <- X[,(i+195)]
  X2[,(4*i+4)] <- X[,(i+292)]
  
}

# 확인 
X2[,1] %>% head() == X[,1] %>% head()   # TRUE
X2[,2] %>% head() == X[,98] %>% head() # TRUE
X2[,3] %>% head() == X[,195] %>% head() # TRUE
X2[,4] %>% head() == X[,292] %>% head() # TRUE

# Same as X.out which is our test set

X.out2 <- X.out %>% replace(!0, 0)

for(i in 0:96){
  
  X.out2[(4*i+1)] <- X.out[(i+1)]
  X.out2[(4*i+2)] <- X.out[(i+98)]
  X.out2[(4*i+3)] <- X.out[(i+195)]
  X.out2[(4*i+4)] <- X.out[(i+292)]
  
}

X.out2 %>% head() # t CPI, t-1 CPI, t-2 CPI, t-3 CPI, / t PCE, t-1 PCE ...
Y3 %>% tail() # Compare


# ================================================
# LSTM 3D array Conversion (LSTM 코드 구조상 이렇게 해줘야함)

X.arr = array(
  data = as.numeric(unlist(X2)),
  dim = c(nrow(X), 4, 97)) # 97 variabes with 4 lagged (time-steps)

X.out.arr = array(
  data = as.numeric(unlist(X.out2)),
  dim = c(1, 4, 97)) # 97 variables with 4 lagged (time-steps)

y %>% head()

X.arr[,,1] %>% head()
Y3[,1] %>% head() # LAG 고려 OK
X.arr[,,2] %>% head()
Y3[,2] %>% head() # LAG 고려 OK

X.arr[,,1] %>% tail()
y %>% tail(1) # 3320807
Y3[,1] %>% tail() # OK

X.out.arr[,,1] # 0.3320807 / 0.3077476 / 0.3078746 / 0.4560760 OK~

# FINAL CHECK COMPLETE

# ==================================================================
# ==================================================================
# ==================================================================
# Multivariate LSTM Model 

run_multi_lstm=function(Y,indice,lag){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2 = cbind(Y, comp$scores[,1:4]) %>% as.data.frame()
  Y3 = lapply(Y2, normalize) %>% as.data.frame() %>% as.matrix()
  aux=embed(Y3,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y3)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y3)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }

  ###
  X2 <- X %>% replace(!0, 0) 
  
  for(i in 0:96){
    X2[,(4*i+1)] <- X[,(i+1)]
    X2[,(4*i+2)] <- X[,(i+98)]
    X2[,(4*i+3)] <- X[,(i+195)]
    X2[,(4*i+4)] <- X[,(i+292)]
  }

  X.out2 <- X.out %>% replace(!0, 0)
  
  for(i in 0:96){
    X.out2[(4*i+1)] <- X.out[(i+1)]
    X.out2[(4*i+2)] <- X.out[(i+98)]
    X.out2[(4*i+3)] <- X.out[(i+195)]
    X.out2[(4*i+4)] <- X.out[(i+292)]
  }

  ###  
  X.arr = array(
    data = as.numeric(unlist(X2)),
    dim = c(nrow(X), 4, 97))
  
  X.out.arr = array(
    data = as.numeric(unlist(X.out2)),
    dim = c(1, 4, 97))
  
  # =============================================================
  # Hyper-Parameters Adjustment

  batch_size = 25  # 25 또는 32 한 번에 입력하는 데이터 크기
  feature = 97  # 설명변수 수
  epochs = 100  # 학습 횟수, 100
  
  model = keras_model_sequential()
  
  # 1-layer model 실행 
  
  model %>% layer_lstm(units = 32, 
                       input_shape = c(4, feature),
                       stateful = FALSE) %>%
    layer_dense(units = 1) 

    
  # 2-layer model with drop out (rate = 0.3) 아래 부분 실행
  
  # model %>% layer_lstm(units = 32,
  #                      input_shape = c(4, feature),
  #                      stateful = FALSE,
  #                      return_sequences = TRUE) %>% 
  #   layer_dropout(rate = 0.3) %>% 
  #   layer_lstm(units = 16) %>% 
  #   layer_dropout(rate = 0.3) %>% 
  #   layer_dense(units = 1)
  
  model %>% compile(loss = 'mse', optimizer = 'adam')
  
  model %>% summary()
  
  history = model %>% fit(X.arr, y, epochs = epochs,
                          batch_size = batch_size, shuffle = FALSE, verbose = 2)
  
  # =============================================================
  
  pred = model(X.out.arr) %>% denormalize(min(Y2[,indice]),max(Y2[,indice])) # De-normalization
  
  return(list("model"=model,"pred"=pred))
}


lstm = run_multi_lstm(Y,1,1)   # 확인용
# ============================================================================




# ============================================================================
# Multivariate Rolling 1 Step ahead LSTM Forecast

mul.lstm.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.pred=matrix(NA,nprev,1)
  
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] %>% as.data.frame()
    lstm=run_multi_lstm(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=as.numeric(lstm$pred) # Note as.numeric()
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors))
}


lstm_1 <- mul.lstm.rolling.window(Y,nprev,1,1)  # forecasting horizon 설정
lstm_3 <- mul.lstm.rolling.window(Y,nprev,1,3)
lstm_6 <- mul.lstm.rolling.window(Y,nprev,1,6)
lstm_12 <- mul.lstm.rolling.window(Y,nprev,1,12)

# Get the error
lstm.rmse = cbind(lstm_1$errors[1], lstm_3$errors[1], lstm_6$errors[1], lstm_12$errors[1])
lstm.mae = cbind(lstm_1$errors[2], lstm_3$errors[2], lstm_6$errors[2], lstm_12$errors[2])

# Get the prediction
lstm.pred = cbind(lstm_1$pred, lstm_3$pred, lstm_6$pred, lstm_12$pred)

# multi_lstm_rolling_ex$errors 

# 0.001883298 RMSE
# 0.001514088 MAE

# saving entire workspace
save.image("results_forecasts(3).RData")

# ============================================================================
# Graphing Test Sets

date_test = seq(as.Date("1990-01-01"),as.Date("2000-12-01"), "months")

real_value = Y[,indice] %>% tail(nprev)

pred_value = multi_lstm_rolling_ex$pred

eval = data.frame(date_test, real_value, pred_value) %>% 
  set_names(c("Date","Actual","Predicted")) %>% 
  reshape2::melt(id="Date")

ggplot(data = eval, aes(x=Date, y=value, colour=variable, group=variable)) +
  geom_line(size=0.5) +
  xlab("") + ylab("") + labs(color = "") +
  scale_x_date(date_breaks ="2 year", date_labels = "%Y-%m") + 
  ggtitle("Multivariate LSTM : Actual vs Predicted") 
