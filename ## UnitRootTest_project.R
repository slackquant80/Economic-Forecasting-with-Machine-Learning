
### Unit Root Test ###

# ADF, PP and KPSS Tests using "urca" package

#getwd()
#setwd("C:/Users/Heejoon/Desktop/R")
#dir()

#install.packages("urca")
#install.packages("readxl")
library(urca)
library(readxl)
library(tidyverse)

data = read.csv("DataKoreaFrom200408updated_WOtcode.csv", header = T)

data1 = data[-1,]

attach(data1)


# Find tcode of each variable: 1) plot, 2) unit root tests

#### 기준년도 100인 데이터 같은 경우 로그 취할 것!!!
#### 금리, 실업률(커봐야 10) 등 스케일 작으면 로그 안취하고

## PI_CA, SI_CA, II_CA
y1 = II_CA
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_I, SI_I, II_I 
y1 = II_I
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_CO,	SI_CO, II_CO 
y1 = SI_CO
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_W, SI_W, II_W
y1 = II_W
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_MM, SI_MM, II_MM
y1 = II_MM
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_MQ, SI_MQ, II_MQ
y1 = II_MQ
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_MF, SI_MF, II_MF, SI_E
y1 = SI_E
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## Unemployment, Act_pop_total, Employed_total, Unemployed_3mts, Unemployed_3_6mts, Unemployed_6_12mts, Unemployed_ov6, Unemployed_ov12
y1 = Unemployed_ov12
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

#y = log(y1)
#plot(y, type = 'l')

adf2 = ur.df(y1, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y1, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y1)
plot(y_diff, type = 'l')
  
adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)
  
kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## HousePrice, HousePrice_S, HousePrice_P
y1 = HousePrice
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## BSI_BC, BSI_SG, BSI_P, BSI_FS, BSI_HR
y1 = BSI_BC
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## PI_P, PI_C, PI_SI, PI_PA
y1 = PI_PA
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y1)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## MB, Currency_cir, CBLIAB_DC, M1, M2, CD, CP
y1 = CD
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

#y = log(y1)
#plot(y, type = 'l')

adf2 = ur.df(y1, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y1, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y1)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## YT_1, YT_3, YT_5, YT_10
y1 = YT_10
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

#y = log(y1)
#plot(y, type = 'l')

adf2 = ur.df(y1, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y1, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y1)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## CB_3_AA, BaseRate, CB_3_BBB
y1 = CB_3_BBB
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

#y = log(y1)
#plot(y, type = 'l')

adf2 = ur.df(y1, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y1, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y1)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## FX_US, FX_CH, FX_JP, FX_EU, FX_BR, FX_CA, FX_SW
y1 = FX_SW
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

## CPI, CPI_F, CPI_C, CPI_H, CPI_HE, CPI_T, CPI_E, CPI_M
y1 = CPI_E
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff_2 <- diff(y_diff)
plot(y_diff_2, type = 'l')

adf2 = ur.df(y_diff_2, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff_2, type = "mu", lags = "long")
kpss2
summary(kpss2)

## SPI_R, SPI_I, SPI_F, SPI_RI, IPI_A, IPI_B, IPI_C, IPI_L, IPI_M, EPI_A, EPI_M
y1 = EPI_M
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "trend")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "tau", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff_2 <- diff(y_diff)
plot(y_diff_2, type = 'l')

adf2 = ur.df(y_diff_2, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff_2, type = "mu", lags = "long")
kpss2
summary(kpss2)

## KOSPI, KOSDAQ, Expected_inf, Oilprice, TB_spread, USCPI, IR_House, USB_10, LeadingCI, LaggingCI, VKOSPI
y1 = VKOSPI
y1 = na.omit(y1) # if necessary

plot(y1, type = 'l')

y = log(y1)
plot(y, type = 'l')

adf2 = ur.df(y, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff <- diff(y)
plot(y_diff, type = 'l')

adf2 = ur.df(y_diff, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff, type = "mu", lags = "long")
kpss2
summary(kpss2)

y_diff_2 <- diff(y_diff)
plot(y_diff_2, type = 'l')

adf2 = ur.df(y_diff_2, type = "drift")
adf2
summary(adf2)

kpss2 = ur.kpss(y_diff_2, type = "mu", lags = "long")
kpss2
summary(kpss2)