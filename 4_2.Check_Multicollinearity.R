rm(list=ls())

# 원본 데이터 가져오기
# install.packages("openxlsx")
library(openxlsx)
full_data = read.xlsx("C:/Users/User/Desktop/2024 환경데이터 활용 및 분석 공모전/데이터/0.전체데이터.xlsx")
data = full_data[1:230, -c(1, 2, 3)]
fillter_data <- data[,c(1, 2, 3, 4, 5, 9, 10, 15, 17)]

# 다중공선성 확인(다중 회귀)(Multiple Regression Analysis)
##  다중공선성 확인 - 상관행렬
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("corrplot")
library(corrplot)
cor_fillter_data = round(cor(fillter_data), 1)
## 첫번째 상관행렬 그래프
ggcorrplot(cor_fillter_data)
## 두번째 상관행렬 그래프
corrplot(cor_fillter_data, method='shade', shade.col=NA, tl.col='black', tl.srt=45)
## 세번째 상관행렬 그래프(채택)
corrplot(cor_fillter_data, method='shade',type="lower", shade.col=NA, tl.col='black', tl.srt=15, tl.cex = 0.7)


# 다중공선성 존재 확인 - vIF(분산팽창인자)
## 변수 지정
livestock = as.numeric(data[, 1])
food_waste = as.numeric(data[, 2])
agriculture = as.numeric(data[, 3])
inflow_water = as.numeric(data[, 4])
throughput_water = as.numeric(data[, 5])
industry = as.numeric(data[, 6])
biogas = as.numeric(data[, 7])
hate_facility = as.numeric(data[, 8])
sewage_treatment = as.numeric(data[, 9])
sewage_waste = as.numeric(data[, 10])
manure_treatment = as.numeric(data[, 11])
food_resource_conversion = as.numeric(data[, 12])
power_station = as.numeric(data[, 13])
power_generation = as.numeric(data[, 14])
gas_company = as.numeric(data[, 15])
hydrogen_shipping_center = as.numeric(data[, 16])
CNG_charging = as.numeric(data[, 17])
hydrogen_charging = as.numeric(data[, 18])
commercial_factory = as.numeric(data[, 19])
## 다중회귀분석
### 종속변수:바이오가스 시설의 수 / 독립변수:나머지 18개
lm = lm(biogas ~ livestock
        +food_waste
        +agriculture
        +inflow_water
        +throughput_water
        +sewage_treatment
        +sewage_waste
        +gas_company
        +CNG_charging, data=fillter_data)
summary(lm) # 다중회귀분석 결과
## vIF
# install.packages("regclass")
library(regclass)
VIF(lm) # VIF >= 10인 경우 다중공선성 존재한다고 판단, 제거한다.
### 결론 : 제거되는 변수가 없음


# 다중공선성 존재 확인 - CI(상태지수)
eig = eigen(cor_fillter_data)
sqrt(max(eig$values)/eig$values) # 상태지수 >= 15인 경우 다중공선성 존재한다고 판단, 제거한다.
### 결론 : 제거되는 변수가 없음