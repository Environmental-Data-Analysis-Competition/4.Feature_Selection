rm(list=ls())

# 원본 데이터 가져오기
# install.packages("openxlsx")
library(openxlsx)
full_data = read.xlsx("C:/Users/User/Desktop/2024 환경데이터 활용 및 분석 공모전/데이터/0.전체데이터.xlsx")
data = full_data[1:230, -c(1, 2, 3)]


## 변수선택법
data2 = data[1:230, -c(7)]
biogas = as.numeric(data[, 7])
forw = lm(biogas~1, data=data2)
back = lm(biogas~., data=data2)
## forward 전진선택법
step(forw, direction = "forward", scope=list(lower=forw, upper=back))
## backward 후진선택법 변수선택법
step(back, direction = "backward")
## stepwise 단계선택법 변수선택법(선택)
step(forw, direction = "both", scope=list(upper=back))



## BIC
### 변수(1, 2, 3, 4, 5, 9, 10, 15, 17)
lm_1 = lm(biogas ~ as.numeric(data[, 1]) + as.numeric(data[, 2]) + as.numeric(data[, 3]) + as.numeric(data[, 4]) 
          + as.numeric(data[, 5]) + as.numeric(data[, 9]) + as.numeric(data[, 10]) + as.numeric(data[, 15]) 
          + as.numeric(data[, 17]) , data=data)
### 변수(1)
lm_2 = lm(biogas ~ as.numeric(data[, 1]), data=data)
### 변수(1,2,3,4,5,6)
lm_3 = lm(biogas ~ as.numeric(data[, 1]) + as.numeric(data[, 2]) + as.numeric(data[, 3]) + as.numeric(data[, 4]) 
          + as.numeric(data[, 5]) + as.numeric(data[, 6]), data=data)
### 변수(8,9,10,11,12)
lm_4 = lm(biogas ~ as.numeric(data[, 8]) + as.numeric(data[, 9]) + as.numeric(data[, 10]) 
          + as.numeric(data[, 11]) + as.numeric(data[, 12]), data=data)
### 변수(13,14,15,16,17,18,19)
lm_5 = lm(biogas ~ as.numeric(data[, 13]) + as.numeric(data[, 14]) + as.numeric(data[, 15]) + as.numeric(data[, 16]) 
          + as.numeric(data[, 17]) + as.numeric(data[, 18]) + as.numeric(data[, 19]), data=data)
### 변수(2,4,5,14)
lm_6 = lm(biogas ~ as.numeric(data[, 2]) + as.numeric(data[, 4]) + as.numeric(data[, 5]) + as.numeric(data[, 14]), data=data)
### 변수(1,3,6,8,9,10,11,12,13,15,16,17,18,19)
lm_7 = lm(biogas ~ as.numeric(data[, 1]) + as.numeric(data[, 3]) + as.numeric(data[, 6]) + as.numeric(data[, 8]) 
          + as.numeric(data[, 9]) + as.numeric(data[, 10]) + as.numeric(data[, 11]) + as.numeric(data[, 12])
          + as.numeric(data[, 13]) + as.numeric(data[, 14]) + as.numeric(data[, 15]) + as.numeric(data[, 16])
          + as.numeric(data[, 17]) + as.numeric(data[, 18]) + as.numeric(data[, 19]), data=data)
BIC(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7)



## R, R_adj
### 변수(1, 2, 3, 4, 5, 9, 10, 15, 17)
summary(lm_1)
### 변수(1)
summary(lm_2)
### 변수(1,2,3,4,5,6)
summary(lm_3)
### 변수(8,9,10,11,12)
summary(lm_4)
### 변수(13,14,15,16,17,18,19)
summary(lm_5)
### 변수(2,4,5,14)
summary(lm_6)
### 변수(1,3,6,8,9,10,11,12,13,15,16,17,18,19)
summary(lm_7)



## 최종 결정된 데이터
fillter_data <- data[,c(1, 2, 3, 4, 5, 9, 10, 15, 17)]
head(fillter_data)
